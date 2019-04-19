with Ada.Text_IO;              use Ada.Text_IO;
with Interfaces;               use Interfaces;

with GNAT.Regpat;
with GNAT.Strings;

with GNATCOLL.Utils;
with GNATCOLL.Strings;
with GNATCOLL.VFS;   use GNATCOLL.VFS;

with Utils;          use Utils;
with Number_Input;   use Number_Input;

package body Device is

   use type GNAT.Strings.String_List_Access;

   package Tmplt renames Templates_Parser;

   function To_Number_Of_Interrupt (Str : String) return Natural;
   function Resources_Base_Directory return String;
   function Arch_From_CPU (CPU_Name : String) return String;

   -------------
   -- Convert --
   -------------

   function Convert (Str : String) return Float_Type is
   begin
      return Float_Type'Value (Str);
   exception
         when Constraint_Error => return Soft;
   end Convert;

   ----------------------------
   -- To_Number_Of_Interrupt --
   ----------------------------

   function To_Number_Of_Interrupt (Str : String) return Natural is
   begin
      return Natural'Value (Str);
   exception
         when Constraint_Error => return 0;
   end To_Number_Of_Interrupt;
   ------------------------------
   -- Resources_Base_Directory --
   ------------------------------

   function Resources_Base_Directory return String is
      Exec_Loc : constant String := GNATCOLL.Utils.Executable_Location;
   begin
      return GNATCOLL.Utils.Join_Path
        (Exec_Loc, "share", "gpr2ld", "resources");
   end Resources_Base_Directory;

   -------------------
   -- Arch_From_CPU --
   -------------------

   function Arch_From_CPU (CPU_Name : String) return String is

      function Match (Pattern : String) return Boolean
      is (GNAT.Regpat.Match (GNAT.Regpat.Compile (Pattern,
                                                  GNAT.Regpat.Case_Insensitive),
                             CPU_Name));

   begin

      if Match ("^cortex-m(0(\+|plus)?|1)$") then
         return "armv6-m";
      elsif Match ("^cortex-m3$") then
         return "armv7-m";
      elsif Match ("^cortex-m(4|7)(f|d)?$") then
         return "armv7e-m";
      elsif Match ("^cortex-m(23|33)(f|d)?$") then
         return "armv8-m";
      end if;
      raise Program_Error with "Unknown CPU name: '" & CPU_Name & "'";
   end Arch_From_CPU;

   ----------------------------------
   -- Get_Memory_List_From_Project --
   ----------------------------------

   procedure Get_Memory_List_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type)
   is
      use Memory_Region_Vectors;

      Memory_List : constant Attribute_Pkg_List :=
        Build (Prj_Package_Name, "memories");
      Size_Table : constant Attribute_Pkg_String :=
        Build (Prj_Package_Name, "Size");
      Address_Table : constant Attribute_Pkg_String :=
        Build (Prj_Package_Name, "Address");
      Kind_Table : constant Attribute_Pkg_String :=
        Build (Prj_Package_Name, "mem_kind");

      Memories : constant GNAT.Strings.String_List_Access :=
        Spec_Project.Attribute_Value (Memory_List);
   begin
      if Memories = null then
         Put_Line ("No memory delcared");
      else
         for Memory of Memories.all loop
            declare
               Size : constant String :=
                 Spec_Project.Attribute_Value (Size_Table, Index => Memory.all);

               Address : constant String :=
                 Spec_Project.Attribute_Value
                   (Address_Table, Index => Memory.all);

               Kind : constant String :=
                 Spec_Project.Attribute_Value (Kind_Table, Index => Memory.all);

               Memory_Unit : constant Memory_Region :=
                 (Name    => To_Unbounded_String (Memory.all),
                  Address => To_Unbounded_String (Address),
                  Size    => To_Unbounded_String (Size),
                  Kind    => Memory_Kind'Value (Kind));
            begin
               Self.Memory := Self.Memory & Memory_Unit;
            end;
         end loop;
      end if;
   end Get_Memory_List_From_Project;

   ---------------------------------------
   -- Get_Interrupt_Vector_From_Project --
   ---------------------------------------

   procedure Get_Interrupt_Vector_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type)
   is
      Interrupts : constant Attribute_Pkg_String :=
        Build
           (Package_Name   => Prj_Package_Name,
            Attribute_Name => "Interrupt");

      Interrupt_List : GNAT.Strings.String_List :=
        Spec_Project.Attribute_Indexes (Interrupts);
   begin
      for Interrupt of Interrupt_List loop
         declare
            Name : constant String :=
              Spec_Project.Attribute_Value
                        (Attribute => Interrupts,
                         Index     => Interrupt.all);

            Interrupt_Index : constant Integer :=
              Integer'Value (Interrupt.all);
         begin
            if Self.Interrupts.Is_Index_Used (Interrupt_Index) then
               --  Key already present, there is a problem in the
               --  project file describing the interrupt vector.
               raise Name_Error
                 with "Interrupt nb " & Interrupt.all &
                      " is already present in the interrupt vector.";
            else
               Self.Interrupts.Add_Interrupt
                  (Interrupt_Index, To_Unbounded_String (Name));
            end if;
         end;
      end loop;
      GNATCOLL.Utils.Free (Interrupt_List);
   end Get_Interrupt_Vector_From_Project;

   ----------------------------------
   -- Get_Boot_Memory_From_Project --
   ----------------------------------

   procedure Get_Boot_Memory_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type)
   is
      Boot_Mem : constant String := Spec_Project.Attribute_Value
         (Build (Prj_Package_Name, "Boot_Memory"));
   begin
      Self.Boot_Memory := To_Unbounded_String (Boot_Mem);

      if Self.Boot_Memory = "" then
         Fatal_Error ("No boot memory specified");
      end if;
   end Get_Boot_Memory_From_Project;

   --------------------------
   -- Get_CPU_From_Project --
   --------------------------

   procedure Get_CPU_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type)
   is
      Name : constant Unbounded_String :=
        To_Unbounded_String
            (Spec_Project.Attribute_Value (Build (Prj_Package_Name, "name")));

      Arch : constant String := Arch_From_CPU (To_String (Name));

      Float_Handling : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "float_handling"));

      Number_Of_Interrupts : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "number_of_interrupts"));

      Linker_Template : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "linker_template"));
      Startup_Template : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "startup_template"));
   begin

      Self.CPU :=
        (Name                 => Name,
         Float_Handling       => Convert (Float_Handling),
         Number_Of_Interrupts => To_Number_Of_Interrupt (Number_Of_Interrupts),
         Arch                 => To_Unbounded_String (Arch));

      if Linker_Template /= "" then
         Self.Linker_Template := To_Unbounded_String (Linker_Template);
      else
         Self.Linker_Template :=
           To_Unbounded_String (Self.Default_Linker_Template);
      end if;

      if Startup_Template /= "" then
         Self.Startup_Template := To_Unbounded_String (Startup_Template);
      else
         Self.Startup_Template
           := To_Unbounded_String (Self.Default_Startup_Template);
      end if;

   end Get_CPU_From_Project;

   -----------
   -- Valid --
   -----------

   function  Valid (Self : in out Spec) return Boolean is
   begin
      --  NOTE: In the case where we have 2 interrupts with the same
      --  attribute number, we will see only the last one due to how
      --  GNATCOLL handles indexed values.
      return Self.Valid_Input and then  Self.Valid_Memory_Regions;
   end Valid;

   -------------
   -- Display --
   -------------

   procedure Display (Self : in out Spec) is
   begin
      Put_Line ("CPU: " & To_String (Self.CPU.Name));
      Put_Line
        ("Float_Handling: " & Float_Type'Image (Self.CPU.Float_Handling));
      for Memory of Self.Memory loop
         Put_Line ("Name    : " & To_String (Memory.Name));
         Put_Line ("Address : " & To_String (Memory.Address));
         Put_Line ("Size    : " & To_String (Memory.Size));
         Put_Line ("Kind    : " & Memory_Kind'Image (Memory.Kind));
      end loop;
   end Display;

   ------------------------
   -- Dump_Linker_Script --
   ------------------------

   procedure Dump_Linker_Script (Self : in out Spec; Filename : String)
   is
      Translations : constant Tmplt.Translate_Table := Self.To_Translate_Table;
      Template     : constant String := To_String (Self.Linker_Template);
      Output       : constant String := Tmplt.Parse (Template, Translations);
      File         : Writable_File :=
        Write_File (Create (Filesystem_String (Filename)));
   begin
      GNATCOLL.VFS.Write (File, Output);
      GNATCOLL.VFS.Close (File);
   end Dump_Linker_Script;

   -----------------------
   -- Dump_Startup_Code --
   -----------------------

   procedure Dump_Startup_Code (Self : in out Spec; Filename : String)
   is
      Translations : constant Tmplt.Translate_Table := Self.To_Translate_Table;
      Template     : constant String := To_String (Self.Startup_Template);
      Output       : constant String := Tmplt.Parse (Template, Translations);
      File         : Writable_File :=
        Write_File (Create (Filesystem_String (Filename)));
   begin
      GNATCOLL.VFS.Write (File, Output);
      GNATCOLL.VFS.Close (File);
   end Dump_Startup_Code;

   ------------------------
   -- To_Translate_Table --
   ------------------------

   function To_Translate_Table
     (Self : Spec)
      return Tmplt.Translate_Table
   is
      use type Tmplt.Vector_Tag;

      Boot_Mem      : Tmplt.Tag;
      Boot_Mem_Addr : Tmplt.Tag;
      Boot_Mem_Size : Tmplt.Tag;

      Main_RAM      : Tmplt.Tag;
      Main_RAM_Addr : Tmplt.Tag;
      Main_RAM_Size : Tmplt.Tag;

      RAM_Regions : Tmplt.Vector_Tag;
      RAM_Addr    : Tmplt.Vector_Tag;
      RAM_Size    : Tmplt.Vector_Tag;

      ROM_Regions : Tmplt.Vector_Tag;
      ROM_Addr    : Tmplt.Vector_Tag;
      ROM_Size    : Tmplt.Vector_Tag;

      Default_Stack_Size : constant Tmplt.Tag := +(2 * 1024);

      Interrupt_Names : Tmplt.Vector_Tag;
      Interrupt_Ids   : Tmplt.Vector_Tag;

      Addr, Size : Unsigned_64;
   begin

      --  First search for the boot memory
      for Mem of Self.Memory loop
         if Mem.name = Self.Boot_Memory then
            Boot_Mem      := +Mem.Name;

            Addr := Convert (Mem.Address);
            Size := Convert (Mem.Size);
            Boot_Mem_Addr := +To_C_Hexadecimal (Addr);
            Boot_Mem_Size := +To_C_Hexadecimal (Size);

            if Mem.Kind = RAM then
               --  Set the main RAM as the boot memory
               Main_RAM      := +Mem.Name;
               Main_RAM_Addr := +To_C_Hexadecimal (Addr);
               Main_RAM_Size := +To_C_Hexadecimal (Size);
            end if;
         end if;
      end loop;

      --  Then set the other memories
      for Mem of Self.Memory loop
         if Mem.name /= Self.Boot_Memory then

            Addr := Convert (Mem.Address);
            Size := Convert (Mem.Size);

            case Mem.Kind is
            when RAM =>
               if Templates_Parser.Size (Main_RAM) = 0 then
                  --  If not already set, use the first in the list RAM in the
                  --  list as Main RAM.
                  Main_RAM      := +Mem.Name;
                  Main_RAM_Addr := +To_C_Hexadecimal (Addr);
                  Main_RAM_Size := +To_C_Hexadecimal (Size);
               else
                  RAM_Regions := RAM_Regions & Mem.Name;
                  RAM_Addr    := RAM_Addr & To_C_Hexadecimal (Addr);
                  RAM_Size    := RAM_Size & To_C_Hexadecimal (Size);
               end if;
            when ROM =>
               ROM_Regions := ROM_Regions & Mem.Name;
               ROM_Addr    := ROM_Addr & To_C_Hexadecimal (Addr);
               ROM_Size    := ROM_Size & To_C_Hexadecimal (Size);
            end case;
         end if;
      end loop;

      for Int_Id in 0 .. Integer'Max (Self.Interrupts.Last_Index,
                                      Self.CPU.Number_Of_Interrupts - 1)
      loop
         Interrupt_Ids := Interrupt_Ids & Int_Id;
         if Self.Interrupts.Is_Index_Used (Int_Id) then
            Interrupt_Names := Interrupt_Names & Self.Interrupts.Get_Name (Int_Id);
            Ada.Text_IO.Put_Line (Self.Interrupts.Get_Name (Int_Id));
         else
            Interrupt_Names := Interrupt_Names & "unknown_interrupt";
            Ada.Text_IO.Put_Line ("unknown_interrupt");
         end if;
      end loop;

      return (Templates_Parser.Assoc ("BOOT_FROM_ROM", Self.Boot_From_ROM),
              Templates_Parser.Assoc ("BOOT_MEM", Boot_Mem),
              Templates_Parser.Assoc ("BOOT_MEM_ADDR", Boot_Mem_Addr),
              Templates_Parser.Assoc ("BOOT_MEM_SIZE", Boot_Mem_Size),
              Templates_Parser.Assoc ("MAIN_RAM", Main_RAM),
              Templates_Parser.Assoc ("MAIN_RAM_ADDR", Main_RAM_Addr),
              Templates_Parser.Assoc ("MAIN_RAM_SIZE", Main_RAM_Size),
              Templates_Parser.Assoc ("RAM_REGION", RAM_Regions),
              Templates_Parser.Assoc ("RAM_ADDR", RAM_Addr),
              Templates_Parser.Assoc ("RAM_SIZE", RAM_Size),
              Templates_Parser.Assoc ("ROM_REGION", ROM_Regions),
              Templates_Parser.Assoc ("ROM_ADDR", ROM_Addr),
              Templates_Parser.Assoc ("ROM_SIZE", ROM_Size),
              Templates_Parser.Assoc ("DEFAULT_STACK_SIZE", Default_Stack_Size),
              Templates_Parser.Assoc ("INTERRUPT_NAME", Interrupt_Names),
              Templates_Parser.Assoc ("INTERRUPT_ID", Interrupt_Ids));
   end To_Translate_Table;

  -------------------
   -- Add_Interrupt --
   -------------------

   procedure Add_Interrupt
      (Self  : in out Interrupt_Vector;
       Index : Integer;
       Name  : Unbounded_String)
   is
   begin
      Interrupt_Hashed_Maps.Include
         (Container => Self.Interrupts,
          Key       => Index,
          New_Item  => Name);

      if Index > Self.Last_Index then
         Self.Last_Index := Index;
      end if;

   end Add_Interrupt;

   -------------------
   -- Is_Index_Used --
   -------------------

   function Is_Index_Used
      (Self  : Interrupt_Vector;
       Index : Integer) return Boolean
   is
      use Interrupt_Hashed_Maps;
      Is_Used : constant Boolean :=
        Find
           (Container => Self.Interrupts,
            Key       => Index) /= No_Element;
   begin
      return Is_Used;
   end Is_Index_Used;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self  : Interrupt_Vector;
       Index : Integer) return String
   is
   begin
      return To_String
         (Interrupt_Hashed_Maps.Element
            (Container => Self.Interrupts,
             Key       => Index));
   end Get_Name;

   -----------------
   -- Valid_Input --
   -----------------

   function Valid_Input (Self : in out Spec) return Boolean is
      Boot_Mem_Is_Valid : Boolean := False;

      Result : Boolean := True;
   begin
      for Memory_Region of Self.Memory loop
         if Memory_Region.Name = Self.Boot_Memory then
            --  We found the boot memory.
            Boot_Mem_Is_Valid := True;

            Self.Boot_From_ROM := Memory_Region.Kind = ROM;
         end if;

         --  If the size or the memory are not matching, we raise an exception.
         if not Number_Input.Valid (To_String (Memory_Region.Size)) then
            Error ("Invalid memory size expression for '" &
                     To_String (Memory_Region.Name) & "' : '" &
                     To_String (Memory_Region.Size) & "'");
            Result := False;
         end if;

         if not Number_Input.Valid (To_String (Memory_Region.Address)) then
            Error ("Invalid memory address expression for '" &
                     To_String (Memory_Region.Name) & "' : '" &
                     To_String (Memory_Region.Address) & "'");
            Result := False;
         end if;
      end loop;

      if not Boot_Mem_Is_Valid then
         Error ("Invalid boot memory : " & To_String (Self.Boot_Memory));
         Result := False;
      end if;

      return Result;
   end Valid_Input;

   --------------------------
   -- Valid_Memory_Regions --
   --------------------------

   function Valid_Memory_Regions (Self : in out Spec) return Boolean is
      Result : Boolean := True;
   begin
      for Region of Self.Memory loop
         for Memory_Region_To_Check_Against of Self.Memory loop
            --  We dont check the memory region against itself.
            if Memory_Region_To_Check_Against.Name /= Region.Name then
               if Memory_Regions_Overlap
                  (Region,
                   Memory_Region_To_Check_Against)
               then
                  Error
                    ("Memory overlap : " &
                       ASCII.LF & Get_Info_String (Region) &
                       ASCII.LF &
                       Get_Info_String (Memory_Region_To_Check_Against));
                  Result := False;
               end if;
            end if;
         end loop;
      end loop;
      return Result;
   end Valid_Memory_Regions;

   ----------------------------
   -- Memory_Regions_Overlap --
   ----------------------------

   function Memory_Regions_Overlap (Memory_1 : Memory_Region;
                                    Memory_2 : Memory_Region)
                                    return Boolean
   is
      Memory_1_Address : constant Unsigned_64 := Convert (Memory_1.Address);
      Memory_1_Size    : constant Unsigned_64 := Convert (Memory_1.Size);
      Memory_2_Address : constant Unsigned_64 := Convert (Memory_2.Address);
      Memory_2_Size    : constant Unsigned_64 := Convert (Memory_2.Size);

   begin
      -- Memory size cannot be zero.
      if Memory_2_Size = 0 or else Memory_1_Size = 0 then
         return True;
      end if;
      if Memory_2_Address > Memory_1_Address then
         return not (Memory_1_Address + Memory_1_Size <= Memory_2_Address);
      elsif Memory_2_Address < Memory_1_Address then
         return not (Memory_2_Address + Memory_2_Size <= Memory_1_Address);
      else --  Memory addresses are the same.
         return True;
      end if;
   end Memory_Regions_Overlap;

   ---------------------
   -- Get_Info_String --
   ---------------------

   function Get_Info_String (Region : Memory_Region) return String is
   begin
      return To_String (Region.Name) & " : Size = " & To_String (Region.Size) &
               "  and Address = " & To_String (Region.Address);
   end Get_Info_String;

   -----------------------------
   -- Default_Linker_Template --
   -----------------------------

   function Default_Linker_Template (Self : Spec) return String is
      use GNATCOLL.Utils;

      Arch : constant String := Arch_From_CPU (To_String (Self.CPU.Name));
   begin
      if Arch = "armv6-m" or else Arch = "armv7-m" or else
         Arch = "armv7e-m" or else Arch = "armv8-m"
      then
         return Join_Path (Resources_Base_Directory, "armvX-m.ld.tmplt");
      end if;

      Fatal_Error ("No default linker template for this configuration, " &
                     "please specify a custom template.");
   end Default_Linker_Template;

   ------------------------------
   -- Default_Startup_Template --
   ------------------------------

   function Default_Startup_Template (Self : Spec) return String is
      use GNATCOLL.Utils;

      Arch : constant String := Arch_From_CPU (To_String (Self.CPU.Name));
   begin
      if Arch = "armv6-m" or else Arch = "armv7-m" or else
         Arch = "armv7e-m" or else Arch = "armv8-m"
      then
         return Join_Path (Resources_Base_Directory, "armvX-m.S.tmplt");
      end if;

      Fatal_Error ("No default startup template for this configuration, " &
                     "please specify a custom template.");
   end Default_Startup_Template;

end Device;
