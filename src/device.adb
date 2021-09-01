------------------------------------------------------------------------------
--                                                                          --
--                               startup-gen                                --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;
with Interfaces;               use Interfaces;

with GNAT.Regpat;
with GNAT.Strings;

with GNATCOLL.Utils;
with GNATCOLL.VFS;   use GNATCOLL.VFS;

with Templates_Parser.Query;
with Templates_Parser.Utils;

with Utils;          use Utils;
with Number_Input;   use Number_Input;

package body Device is

   use type GNAT.Strings.String_List_Access;

   package Tmplt renames Templates_Parser;

   function To_Number_Of_Interrupt (Str : String) return Natural;
   function Resources_Base_Directory return String;
   function Arch_From_CPU (CPU_Name : String) return String;
   function Arch_From_Runtime (Runtime_Name : String) return String;
   function C_Comment_Box_Filter
     (Value      : String;
      Parameters : String;
      Context    : Templates_Parser.Filter_Context)
      return String;

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
        (Exec_Loc, "share", "startup-gen", "resources");
   end Resources_Base_Directory;

   -------------------
   -- Arch_From_CPU --
   -------------------

   function Arch_From_CPU (CPU_Name : String) return String is

      function Match (Pattern : String) return Boolean
      is (GNAT.Regpat.Match (GNAT.Regpat.Compile
                             (Pattern,
                              GNAT.Regpat.Case_Insensitive),
                             CPU_Name));

   begin
      if CPU_Name = "" then
         return "";
      end if;

      if Match ("^(arm)?\s*cortex-m(0(\+|plus|p)?|1)$") then
         return "armv6-m";
      elsif Match ("^(arm)?\s*cortex-m3$") then
         return "armv7-m";
      elsif Match ("^(arm)?\s*cortex-m(4|7)(f|d|fd)?$") then
         return "armv7e-m";
      elsif Match ("^(arm)?\s*cortex-m(23|33)(f|d|fd)?$") then
         return "armv8-m";
      elsif Match ("^(riscv|risc-v|rv)(32|64|128)?$") then
         return "risc-v";
      end if;

      Fatal_Error ("Unknown CPU name: '" & CPU_Name & "'");
   end Arch_From_CPU;

   function Arch_From_Runtime (Runtime_Name : String) return String is

      function Match (Pattern : String) return Boolean
      is (GNAT.Regpat.Match (GNAT.Regpat.Compile
                             (Pattern,
                              GNAT.Regpat.Case_Insensitive),
                             Runtime_Name));

   begin
      if Runtime_Name = "" then
         return "";
      end if;

      if Match ("^(light|zfp)-cortex-m(0(\+|plus|p)?|1)$") then
         return "armv6-m";
      elsif Match ("^(light|zfp)-cortex-m3$") then
         return "armv7-m";
      elsif Match ("^(light|zfp)-cortex-m(4|7)(f|d|fd)?$") then
         return "armv7e-m";
      elsif Match ("^(light|zfp)-cortex-m(23|33)(f|d|fd)?$") then
         return "armv8-m";
      elsif Match ("^(light|zfp)-(riscv|risc-v|rv)(32|64|128)?[imafdgqlcjtpvnh]*$") then
         return "risc-v";
      end if;

      Warning ("Unknown run-time name: '" & Runtime_Name & "'");
      return "";
   end Arch_From_Runtime;

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
                 Spec_Project.Attribute_Value
                 (Size_Table, Index => Memory.all);

               Address : constant String :=
                 Spec_Project.Attribute_Value
                   (Address_Table, Index => Memory.all);

               Kind_Str : constant String :=
                 Spec_Project.Attribute_Value
                 (Kind_Table, Index => Memory.all);

            begin

               if Size = "" then
                  Fatal_Error
                    ("Missing Size for memory: '" & Memory.all & "'");
               elsif Address = "" then
                  Fatal_Error
                    ("Missing Address for memory: '" & Memory.all & "'");
               elsif Kind_Str = "" then
                  Fatal_Error
                    ("Missing Kind for memory: '" & Memory.all & "'");
               end if;

               begin
                  declare
                     Kind : constant Memory_Kind :=
                       Memory_Kind'Value (Kind_Str);
                  begin
                     Self.Memory := Self.Memory &
                     (Name    => To_Unbounded_String (Memory.all),
                      Address => To_Unbounded_String (Address),
                      Size    => To_Unbounded_String (Size),
                      Kind    => Kind);
                  end;
               exception
                  when Constraint_Error =>
                     Fatal_Error ("Invalid memory kind: '" & Kind_Str & "'");
               end;
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
          (Spec_Project.Attribute_Value (Build (Prj_Package_Name,
           "cpu_name")));

      CPU_Arch : constant String := Arch_From_CPU (To_String (Name));

      RTS_Arch : constant String := Arch_From_Runtime
        (Spec_Project.Get_Runtime);

      Arch : Unbounded_String;

      Float_Handling : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "float_handling"));

      Number_Of_Interrupts : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "number_of_interrupts"));

      Main_Stack_Size : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "main_stack_size"));

      Main_Stack_Memory : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "main_stack_memory"));

      Linker_Template : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "linker_template"));
      Startup_Template : constant String :=
        Spec_Project.Attribute_Value (Build (Prj_Package_Name,
                                      "startup_template"));
   begin

      if RTS_Arch = "" then
         if CPU_Arch /= "" then
            Arch := To_Unbounded_String (CPU_Arch);
         else
            Fatal_Error
              ("Unknown CPU, please specify CPU_Name or Rumtime attribute");
         end if;
      else
         if CPU_Arch = "" or else CPU_Arch = RTS_Arch then
            Arch := To_Unbounded_String (RTS_Arch);
         else
            Fatal_Error
              ("Mismatch between CPU_Name (" & To_String (Name) &
                 ") and run-time (" & Spec_Project.Get_Runtime &
                 ") attributes");
         end if;
      end if;

      Self.CPU :=
        (Name                 => Name,
         Float_Handling       => Convert (Float_Handling),
         Number_Of_Interrupts => To_Number_Of_Interrupt (Number_Of_Interrupts),
         Arch                 => Arch);

      if Main_Stack_Size /= "" then
         Self.Main_Stack_Size := To_Unbounded_String (Main_Stack_Size);
      else
         Self.Main_Stack_Size := To_Unbounded_String ("0x1000");
      end if;

      if Main_Stack_Memory /= "" then
         Self.Main_Stack_Memory := To_Unbounded_String (Main_Stack_Memory);
      end if;

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

   --------------------------------
   -- Get_User_Tags_From_Project --
   --------------------------------

   procedure Get_User_Tags_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type)
   is
      User_Tags : constant Attribute_Pkg_String :=
        Build (Package_Name   => Prj_Package_Name,
               Attribute_Name => "user_tag");

      User_Tag_List : GNAT.Strings.String_List :=
        Spec_Project.Attribute_Indexes (User_Tags);
   begin

      for Tag of User_Tag_List loop
         declare
            Name : constant String :=
              Spec_Project.Attribute_Value
                (Attribute => User_Tags,
                 Index     => Tag.all);
         begin
            if Self.User_Tags.Contains (To_Unbounded_String (Tag.all)) then
               Fatal_Error ("User tag '" & Tag.all & "' already defined");
            else
               Self.User_Tags.Insert (To_Unbounded_String (Tag.all),
                                      To_Unbounded_String (Name));
            end if;
         end;
      end loop;
      GNATCOLL.Utils.Free (User_Tag_List);
   end Get_User_Tags_From_Project;

   -----------
   -- Valid --
   -----------

   function  Valid (Self : in out Spec) return Boolean is
   begin
      --  NOTE: In the case where we have 2 interrupts with the same
      --  attribute number, we will see only the last one due to how
      --  GNATCOLL handles indexed values.
      return Self.Valid_Input and then Self.Valid_Memory_Regions;
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

   --------------------------
   -- Dump_Translate_Table --
   --------------------------

   procedure Dump_Translate_Table (Self : in out Spec) is
   begin
      Put_Line ("--- Template tags ---");

      for Assoc of Self.To_Translate_Table loop

         case Tmplt.Query.Kind (Assoc) is

            when Tmplt.Std =>
               Put_Line (Tmplt.Query.Variable (Assoc) & " => " &
                           Tmplt.Get (Assoc));

            when Tmplt.Composite =>
               declare
                  Tag : constant Tmplt.Tag := Tmplt.Get (Assoc);
               begin
                  Put_Line (Tmplt.Query.Variable (Assoc) & " => " &
                              Tmplt.Utils.Image (Tag));
               end;
         end case;
      end loop;

      Put_Line ("---------------------");
   end Dump_Translate_Table;

   ------------------------
   -- To_Translate_Table --
   ------------------------

   function To_Translate_Table
     (Self : Spec)
      return Tmplt.Translate_Table
   is
      use type Tmplt.Vector_Tag;
      use type Tmplt.Translate_Table;

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

      Stack_Size : constant Tmplt.Tag :=
        +To_C_Hexadecimal (Convert (Self.Main_Stack_Size));
      Stack_Region : Tmplt.Tag;

      Interrupt_Names : Tmplt.Vector_Tag;
      Interrupt_Ids   : Tmplt.Vector_Tag;

      Addr, Size : Unsigned_64;

      User_Assocs : Tmplt.Translate_Table
        (1 .. Integer (Self.User_Tags.Length));
      User_Tags : User_Tags_Maps.Map := Self.User_Tags.Copy;

   begin

      --  First search for the boot memory
      for Mem of Self.Memory loop
         if Mem.Name = Self.Boot_Memory then
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
         if Mem.Name /= Self.Boot_Memory then

            Addr := Convert (Mem.Address);
            Size := Convert (Mem.Size);

            case Mem.Kind is

            when RAM =>
               if Templates_Parser.Size (Main_RAM) = 0 then
                  --  If not already set, use the RAM in the list as Main RAM
                  Main_RAM      := +Mem.Name;
                  Main_RAM_Addr := +To_C_Hexadecimal (Addr);
                  Main_RAM_Size := +To_C_Hexadecimal (Size);
               elsif Mem.Name /= Tmplt.Item (Main_RAM, 1) then
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

      --  Select stack region
      if Self.Main_Stack_Memory /= "" then
         if not (for some Mem of Self.Memory
                 => Mem.Name = Self.Main_Stack_Memory)
         then
            Fatal_Error ("Undefined memory used for main stack: '" &
                         To_String (Self.Main_Stack_Memory) & "'");
         end if;

         if (for some Mem of Self.Memory
             => Mem.Name = Self.Main_Stack_Memory and then Mem.Kind = ROM)
         then
            Fatal_Error ("Main stack memory cannot be ROM: '" &
                         To_String (Self.Main_Stack_Memory) & "'");
         end if;

         Stack_Region := +Self.Main_Stack_Memory;
      else
         Stack_Region := Main_RAM;
      end if;

      --  Interrupts
      for Int_Id in 0 .. Integer'Max (Self.Interrupts.Last_Index,
                                      Self.CPU.Number_Of_Interrupts - 1)
      loop
         Interrupt_Ids := Interrupt_Ids & Int_Id;
         if Self.Interrupts.Is_Index_Used (Int_Id) then
            Interrupt_Names := Interrupt_Names &
              Self.Interrupts.Get_Name (Int_Id);
         else
            Interrupt_Names := Interrupt_Names & "unknown_interrupt";
         end if;
      end loop;

      --  User defined tags
      for Assoc of User_Assocs loop
         declare
            Cur : constant User_Tags_Maps.Cursor := User_Tags.First;
            Key : constant Unbounded_String := User_Tags_Maps.Key (Cur);
            Val : constant Unbounded_String := User_Tags_Maps.Element (Cur);
         begin
            Assoc := Templates_Parser.Assoc (To_String (Key),
                                             To_String (Val));
            User_Tags.Delete (Key);
         end;
      end loop;

      return User_Assocs &
              (Templates_Parser.Assoc ("FLOAT_HANDLING",
                                       Self.CPU.Float_Handling'Img),
              Templates_Parser.Assoc ("BOOT_FROM_ROM", Self.Boot_From_ROM),
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
              Templates_Parser.Assoc ("MAIN_STACK_SIZE", Stack_Size),
              Templates_Parser.Assoc ("MAIN_STACK_REGION", Stack_Region),
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

      if not Number_Input.Valid (To_String (Self.Main_Stack_Size)) then
         Error ("Invalid main stack size : " &
                  To_String (Self.Main_Stack_Size));
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
      --  Memory size cannot be zero.
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

      Arch : constant String := To_String (Self.CPU.Arch);
   begin
      if Arch = "armv6-m" or else Arch = "armv7-m" or else
         Arch = "armv7e-m" or else Arch = "armv8-m"
      then
         return Join_Path (Resources_Base_Directory, "armvX-m.ld.tmplt");
      elsif Arch = "risc-v" then
         return Join_Path (Resources_Base_Directory, "riscv.ld.tmplt");
      end if;

      Fatal_Error ("No default linker template for this configuration, " &
                     "please specify a custom template.");
   end Default_Linker_Template;

   ------------------------------
   -- Default_Startup_Template --
   ------------------------------

   function Default_Startup_Template (Self : Spec) return String is
      use GNATCOLL.Utils;

      Arch : constant String := To_String (Self.CPU.Arch);
   begin
      if Arch = "armv6-m" or else Arch = "armv7-m" or else
         Arch = "armv7e-m" or else Arch = "armv8-m"
      then
         return Join_Path (Resources_Base_Directory, "armvX-m.S.tmplt");
      elsif Arch = "risc-v" then
         return Join_Path (Resources_Base_Directory, "riscv.S.tmplt");
      end if;

      Fatal_Error ("No default startup template for this configuration, " &
                     "please specify a custom template.");
   end Default_Startup_Template;

   --------------------------
   -- C_Comment_Box_Filter --
   --------------------------

   function C_Comment_Box_Filter
     (Value      : String;
      Parameters : String;
      Context    : Templates_Parser.Filter_Context)
      return String
   is
      pragma Unreferenced (Context);
      use Ada.Strings.Fixed;

      Separator_Index : constant Natural := Index (Parameters, "/");

      Indent_Cnt : constant Natural := Natural'Value
        ((if Parameters'Length = 0
         then "0"
         elsif Separator_Index /= 0
         then Head (Parameters, Separator_Index - 1)
         else Parameters));

      Prefix : constant String :=
        (if Separator_Index /= 0
         then Tail (Parameters, Parameters'Length - Separator_Index)
         else "");

      Text   : constant String := Prefix & Value;
      Indent : constant String := Indent_Cnt * ' ';
      Bar    : constant String := "/**" & (Text'Length * '*') & "**/";
   begin
      return Indent & Bar & ASCII.LF &
        Indent & "/* " & Text & " */" & ASCII.LF &
        Indent & Bar;
   end C_Comment_Box_Filter;

begin
   Templates_Parser.Register_Filter
     ("C_COMMENT_BOX", C_Comment_Box_Filter'Access);
end Device;
