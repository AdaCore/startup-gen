with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Numerics.Elementary_Functions;

with GNAT.Regexp;
with GNAT.Strings;

with GNATCOLL.Utils;

------------
-- Device --
------------

package body Device is

   ----------------------------------
   -- Get_Memory_List_From_Project --
   ----------------------------------

   procedure Get_Memory_List_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type)
   is
      use Memory_Region_Vectors;

      Memory_List : constant Attribute_Pkg_List :=
        Build ("Memory_map", "memories");
      Size_Table : constant Attribute_Pkg_String :=
        Build ("Memory_map", "Size");
      Address_Table : constant Attribute_Pkg_String :=
        Build ("Memory_map", "Address");
      Kind_Table : constant Attribute_Pkg_String :=
        Build ("Memory_map", "mem_kind");

   begin
      for Memory of Spec_Project.Attribute_Value (Memory_List).all loop
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
               Address => To_Unbounded_String (To_Size_String (Address)),
               Size    => To_Unbounded_String (To_Size_String (Size)),
               Kind    => Memory_Kind'Value (Kind));
         begin
            Self.Memory := Self.Memory & Memory_Unit;
         end;
      end loop;

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
           (Package_Name   => "Interrupt_Vector",
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
         (Build ("Memory_map", "Boot_Memory"));
   begin
      Self.Boot_Memory := To_Unbounded_String (Boot_Mem);
   end Get_Boot_Memory_From_Project;

   --------------------------
   -- Get_CPU_From_Project --
   --------------------------
   procedure Get_CPU_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type)
   is
      use Architecture_Hashed_Maps;
      Name : constant Unbounded_String :=
        To_Unbounded_String
            (Spec_Project.Attribute_Value (Build ("cpu", "name")));

      Float_Handling : constant String :=
        Spec_Project.Attribute_Value (Build ("cpu", "float_handling"));

      Number_Of_Interrupts : constant String :=
        Spec_Project.Attribute_Value (Build ("cpu", "number_of_interrupts"));
   begin

      if Find (Container => Self.Architectures,
               Key       => Name) /= No_Element
      then
         Self.CPU :=
           (Name                 => Name,
            Float_Handling       => Float_Type'Value (Float_Handling),
            Number_Of_Interrupts => Natural'Value (Number_Of_Interrupts),
            Arch                 => Element (Self.Architectures, Name));
      else
         raise Name_Error with
            "Current CPU " & To_String (Self.CPU.Name) & " not supported.";
      end if;
   end Get_CPU_From_Project;

   -------------------------------
   -- Setup_Known_Architectures --
   -------------------------------

   procedure Setup_Known_Architectures
      (Self         : in out Spec;
       Spec_Project : Project_Type;
       Config_Dir   : String)
   is
      Architectures : constant Attribute_Pkg_String :=
        Build
           (Package_Name   => "Architectures_Configuration",
            Attribute_Name => "CPU_Architecture");

      Dir : constant String := Config_Dir & Spec_Project.Attribute_Value
         (Build ("Architectures_Configuration", "Dir"));

      Arch_List : constant GNAT.Strings.String_List :=
        Spec_Project.Attribute_Indexes (Architectures);
   begin
      for Arch of Arch_List loop
         declare
            Name : constant String :=
              Spec_Project.Attribute_Value
                        (Attribute => Architectures,
                         Index     => Arch.all);

            CPU : constant String := (Arch.all);

            Arch : constant Arch_Algorithms :=
               Make_Arch_Algorithms
                  (Directory => Dir,
                   Name      => Name);

         begin
            Architecture_Hashed_Maps.Include
               (Container => Self.Architectures,
                Key       => To_Unbounded_String (CPU),
                New_Item  => Arch);
         end;
      end loop;
   end Setup_Known_Architectures;

   -----------------------
   -- Generate_Sections --
   -----------------------

   procedure Generate_Sections (Self : in out Spec) is
      use Sections;
      use Unbounded_String_Vectors;
      function TUS (Str : String) return Unbounded_String
         renames To_Unbounded_String;

      use Sections.Section_Vectors;

      -----------------
      -- Add_Section --
      -----------------

      procedure Add_Section (Sec : Section) is
      begin
         Self.Section_Vector := Self.Section_Vector & Sec;
      end Add_Section;

   begin

      --  In the long term we want to be able to specify
      --  the sections that corresponds to the target architecture.
      --  For now and for ARM processors, it will do.

      for Mem of Self.Memory loop
         case Mem.Kind is
            when ROM =>
               Add_Section (Make_Section
                            (Boot_Memory        => Self.Boot_Memory,
                             Name               => TUS ("text"),
                             Reloc_Memory       => Self.Boot_Memory,
                             Additional_Content =>
                               (if Mem.Name = Self.Boot_Memory then
                                   Unbounded_String_Vectors.Empty_Vector &
                                   TUS ("KEEP (*(.vectors))")
                                else Unbounded_String_Vectors.Empty_Vector)
                            ));
               Add_Section (Make_Section
                            (Boot_Memory  => Self.Boot_Memory,
                             Name         => TUS ("rodata"),
                             Reloc_Memory => Self.Boot_Memory
                            ));
            when RAM =>
               Add_Section (Make_Section
                            (Boot_Memory  => Self.Boot_Memory,
                             Name         => TUS ("bss"),
                             Reloc_Memory => Mem.Name,
                             Force_Init   => True,
                             Load         => False,
                             Has_Stack    => True,
                             Init_Code    => Self.CPU.Arch.Clear_Code
                            ));

               Add_Section (Make_Section
                            (Boot_Memory  => Self.Boot_Memory,
                             Name         => TUS ("data"),
                             Reloc_Memory => Mem.Name,
                             Init_Code    => Self.CPU.Arch.Copy_Code
                            ));
            when others =>
               null;
         end case;
      end loop;
   end Generate_Sections;

   --------------
   -- Validate --
   --------------

   procedure Validate (Self : in out Spec) is
   begin
      --  NOTE: In the case where we have 2 interrupts with the same
      --  attribute number, we will see only the last one due to how
      --  GNATCOLL handles indexed values.
      Self.Validate_Input;
      Self.Validate_Memory_Regions;
   end Validate;

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

   procedure Dump_Linker_Script (Self : in out Spec; VF : Virtual_File) is
      File : Indented_File_Writer := Make (Handle => Write_File (VF));
   begin
      Dump_Header (File);

      Self.Dump_Memory_Map (File);

      File.New_Line;

      File.Put_Line ("SEARCH_DIR(.)");
      File.New_Line;
      File.Put_Line ("__DYNAMIC = 0;");
      File.New_Line;
      File.Put_Line ("_DEFAULT_STACK_SIZE = 4 * 1024;");

      File.New_Line;
      File.Put_Line ("ENTRY(Reset_Handler);");
      File.New_Line;

      Self.Dump_Sections (File);

      File.Close;
   end Dump_Linker_Script;

   ---------------------
   -- Dump_Memory_Map --
   ---------------------

   procedure Dump_Memory_Map
      (Self : in out Spec;
       File : in out Indented_File_Writer)
   is
   begin

      File.Put_Line ("MEMORY");
      File.Put_Line ("{");
      File.Indent;

      for Memory of Self.Memory loop
         declare
            --  XXX: Hardcoded permissions for now
            Permissions : constant Unbounded_String :=
                     To_Unbounded_String (if Memory.Kind = ROM
                                             then "(rx)"
                                             else "(rwx)");
         begin
            Dump_Memory (File, Memory, Permissions);
         end;
      end loop;

      File.Unindent;
      File.Put_Line ("}");

   end Dump_Memory_Map;

   -----------------------
   -- Dump_Startup_Code --
   -----------------------

   procedure Dump_Startup_Code (Self : in out Spec; VF : Virtual_File)
   is
      File : Indented_File_Writer :=
         Make (Handle                => Write_File (VF),
               Indentation_Size      => 1,
               --  We indent with tab;
               Indentation_Character => ASCII.HT);
   begin
      Dump_Header (File);
      File.New_Line;
      --  XXX: Hardcoded values for ARM assembly.
      File.Indent;
      File.Put_Indented_Line (".syntax unified");
      File.Put_Indented_Line (".cpu " & Self.CPU.Name);
      File.Put_Indented_Line (".thumb");
      File.Unindent;

      File.New_Line;
      File.New_Line;

      Self.Dump_Interrupt_Vector (File);

      File.New_Line;
      File.Put_Indented_Line (".text");
      File.Put_Indented_Line (".thumb_func");
      File.Put_Indented_Line (".globl Reset_Handler");

      File.Put_Line ("Reset_Handler:");
      File.Put_Line ("_start_" & Self.Boot_Memory & ":");
      Self.Dump_Sections_Init_Code (File);

      File.Indent;
      File.New_Line;
      File.Put_Indented_Line ("bl main");
      File.New_Line;
      File.Put_Indented_Line ("bl _exit");
      File.Put_Line ("hang:");
      File.Put_Indented_Line ("b .");
      File.Unindent;

   end Dump_Startup_Code;

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
      (Self  : in out Interrupt_Vector;
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
      (Self  : in out Interrupt_Vector;
       Index : Integer) return String
   is
   begin
      return To_String
         (Interrupt_Hashed_Maps.Element
            (Container => Self.Interrupts,
             Key       => Index));
   end Get_Name;

   -----------------
   -- Dump_Header --
   -----------------

   procedure Dump_Header
      (File : in out Indented_File_Writer)
   is
   begin
      File.New_Line;
      File.Put_Line ("/* This file has been generated by gpr2ld. */");
      File.New_Line;
   end Dump_Header;

   -------------------
   -- Dump_Sections --
   -------------------

   procedure Dump_Sections
      (Self : in out Spec;
       File : in out Indented_File_Writer)
   is
   begin
      File.Put_Line ("SECTIONS");
      File.Put_Line ("{");
      File.Indent;

      for Section of Self.Section_Vector loop
         Self.Dump_Section (File, Section);
         File.New_Line;

         --  XXX: Hardcoded content specific to ARM target.
         --  FIXME: bug with section name
         if Section.Name = "text" then
            File.Put_Indented_Line
            (".ARM.extab   : { *(.ARM.extab* .gnu.linkonce.armextab.*) } > " &
              Self.Boot_Memory);
            File.New_Line;
            File.Put_Indented_Line (".ARM.exidx :");
            File.Put_Indented_Line ("{");
            File.Indent;

            File.Put_Indented_Line ("PROVIDE_HIDDEN (__exidx_start = .);");
            File.Put_Indented_Line ("*(.ARM.exidx* .gnu.linkonce.armexidx.*)");
            File.Put_Indented_Line ("PROVIDE_HIDDEN (__exidx_end = .);");

            File.Unindent;
            File.Put_Indented_Line ("} > " & Self.Boot_Memory);
            File.New_Line;
         end if;
      end loop;

      File.Unindent;
      File.Put_Line ("}");
   end Dump_Sections;

   ------------------
   -- Dump_Section --
   ------------------

   procedure Dump_Section
      (Self    : in out Spec;
       File    : in out Indented_File_Writer;
       Section : in out Sections.Section)
   is
      Load_String : constant Unbounded_String :=
         To_Unbounded_String
            (if not Section.To_Load
             then " (NOLOAD)"
             else "");
      Dot_Name : constant Unbounded_String :=
         To_Unbounded_String (".") & Section.Name;

      Destination_Memory : constant String :=
         (if Self.Boot_Memory /= Section.Reloc_Memory
               and then Section.To_Load
          then (To_String (Section.Reloc_Memory) & (" AT> ") &
                To_String (Self.Boot_Memory))
          else To_String (Section.Reloc_Memory));
   begin

      --  If the section has initalization code, we dump the
      --  symbols required by the startup code.

      File.Put_Indented_Line ("__" & Section.Name & "_load = .;");

      File.Put_Indented_Line (Dot_Name & Load_String & " :");
      File.Put_Indented_Line ("{");
      File.Indent;

      File.Put_Indented_Line ("__" & Section.Name & "_start = .;");

      for Content of Section.Additional_Content loop
         File.Put_Indented_Line (Content);
      end loop;

      File.Put_Indented_Line
         ("*(" & Dot_Name & " " & Dot_Name & ".*)");

      --  XXX: Hardcoded content for the BSS.
      --  Note that we can put the alignement in the section record.
      if Section.Name = "bss" then
         File.Put_Indented_Line ("*(COMMON)");
         File.Put_Indented_Line (". = ALIGN(0x8);");
      else
         File.Put_Indented_Line (". = ALIGN(0x4);");
      end if;

      File.Put_Indented_Line ("__" & Section.Name & "_end = .;");
      File.New_Line;

      if Section.Has_Stack then
         File.New_Line;

         File.Put_Indented_Line ("__stack_start = .;");
         File.Put_Indented_Line (". += DEFINED (__stack_size) ?" &
               " __stack_size : _DEFAULT_STACK_SIZE;");
         File.Put_Indented_Line (". = ALIGN(0x8);");
         File.Put_Indented_Line ("__stack_end = .;");

         File.New_Line;

         File.Put_Indented_Line ("__heap_start = .;");
         File.Put_Indented_Line
            ("__heap_end = ORIGIN(" & Destination_Memory &
             ") + LENGTH(" & Destination_Memory & ");");

         File.New_Line;
      end if;

      File.Unindent;
      File.Put_Indented_Line ("} > " & Destination_Memory);
      File.Put_Indented_Line
         ("__" & Section.Name & "_words = (__" & Section.Name
         & "_end - __" & Section.Name & "_start) >> 2;");
   end Dump_Section;

   -----------------
   -- Dump_Memory --
   -----------------

   procedure Dump_Memory
        (File        : in out Indented_File_Writer;
         Memory      : Memory_Region;
         Permissions : Unbounded_String)
   is
      Name         : constant Unbounded_String  := Memory.Name;
      Address      : constant Unbounded_String  := Memory.Address;
      Size         : constant Unbounded_String  := Memory.Size;
   begin
      File.Put_Indented_Line (Name & " " & Permissions & " : ORIGIN = " &
                              Address & ", LENGTH = " & Size);
   end Dump_Memory;

   ---------------------------
   -- Dump_Interrupt_Vector --
   ---------------------------

   procedure Dump_Interrupt_Vector
      (Self : in out Spec;
       File : in out Indented_File_Writer)
   is
   begin
      File.Indent;
      --  XXX: Hardcoded for now, we will use Self.Interrupt_Vector when
      --  the handling of the interrupt vector is done.
      File.Put_Indented_Line (".section .vectors," & '"' & "a" & '"');
      File.Put_Line ("__vectors:");
      File.Put_Indented_Line ("/* System defined interrupts */");
      File.Put_Indented_Line (".word __stack_end /* top of the stack */");
      Put_Interrupt (File, "Reset");
      Put_Interrupt (File, "NMI");
      Put_Interrupt (File, "Hard_Fault");
      Put_Interrupt (File, "Mem_Manage");
      Put_Interrupt (File, "Bus_Fault");
      Put_Interrupt (File, "Usage_Fault");
      File.Put_Indented_Line (".word 0    /* reserved */");
      File.Put_Indented_Line (".word 0    /* reserved */");
      File.Put_Indented_Line (".word 0    /* reserved */");
      File.Put_Indented_Line (".word 0    /* reserved */");
      Put_Interrupt (File, "SVC");
      Put_Interrupt (File, "Debug_Mon");
      File.Put_Indented_Line (".word 0    /* reserved */");
      Put_Interrupt (File, "Pend_SV");
      Put_Interrupt (File, "SysTick");

      --  We add the interrupts corresponding
      --  to what is in the interrupt vector.
      File.New_Line;
      File.Put_Indented_Line ("/* External interrupts */");
      File.New_Line;

      --  Adjust the number of interrupts, if there is more explicitly declared
      --  interrupt than the CPU.Number_Of_Interrupts. This will also happen if
      --  user doesn't specify Number_Of_Interrupts in the CPU package.
      if Self.Interrupts.Last_Index > Self.CPU.Number_Of_Interrupts then
         Self.CPU.Number_Of_Interrupts := Self.Interrupts.Last_Index + 1;
      end if;

      for I in Integer range 0 .. Self.CPU.Number_Of_Interrupts - 1 loop
         if Self.Interrupts.Is_Index_Used (I) then
            declare
               Name : constant String := Self.Interrupts.Get_Name (I);
               Line : constant String :=
                 ".word " & Name & "_Handler" & "    /*" & I'Img & " " & Name & " */";
            begin
               File.Put_Indented_Line (Line);
            end;
         else
            File.Put_Indented_Line (".word UnnamedInterrupt_Handler    /*" & I'Img & " */");
         end if;
      end loop;

      File.New_Line;

      --  We generate weak aliases that the user can
      --  override by linking again his own implementation.
      --  We dont care about the order in which the symbols are declared.
      Put_Dummy_Handler (File, "SysTick");
      Put_Dummy_Handler (File, "Debug_Mon");
      Put_Dummy_Handler (File, "SVC");
      Put_Dummy_Handler (File, "Pend_SV");
      Put_Dummy_Handler (File, "Usage_Fault");
      Put_Dummy_Handler (File, "Bus_Fault");
      Put_Dummy_Handler (File, "Mem_Manage");
      Put_Dummy_Handler (File, "Hard_Fault");
      Put_Dummy_Handler (File, "NMI");
      Put_Dummy_Handler (File, "UnnamedInterrupt");
      for Cursor in Self.Interrupts.Interrupts.Iterate loop
         declare
            Name : constant String :=
              To_String (Interrupt_Hashed_Maps.Element (Cursor));
            Weak_Symbol : constant String :=
              ".weak" & ASCII.HT & ASCII.HT & Name & "_Handler";
            Override_Symbol : constant String :=
              ".thumb_set" & ASCII.HT & Name & "_Handler,hang";
         begin
            File.Put_Indented_Line (Weak_Symbol);
            File.Put_Indented_Line (Override_Symbol);
            File.New_Line;
         end;
      end loop;

      File.Unindent;
   end Dump_Interrupt_Vector;

   ---------------------------
   -- Dump_Interrupt_Vector --
   ---------------------------

   procedure Dump_Sections_Init_Code
      (Self : in out Spec;
       File : in out Indented_File_Writer)
   is
   begin
      for Section of Self.Section_Vector loop
         if Section.To_Init then

            Section.Init_Code.Format
               (Name   => Section.Name,
                Indent => To_Unbounded_String ("" & ASCII.HT));

            for Line of Section.Init_Code.Get_Lines loop
               File.Put_Line (Line);
            end loop;
            File.New_Line;
         end if;
      end loop;
   end Dump_Sections_Init_Code;

   -----------------------
   -- Is_Based_Litteral --
   -----------------------

   function Is_Based_Literal (Number : String) return Boolean
   is
      use GNAT.Regexp;
      Numeral       : constant String := "([0-9]+)";
      Base          : constant String := "([2-9])|(1[0-6])";
      Based_Numeral : constant String := "(([0-9])|([A-F]|[a-f]))+";

      Exponent      : constant String :=
        "((" & "E-" & Numeral & ")|(" & "E\+?" & Numeral & "))*";

      Pattern       : constant String :=
        Base & "#" & "\.?" & Based_Numeral & "#" & Exponent;

      Expr : constant Regexp := Compile (Pattern => Pattern);
   begin
      return Match (Number, Expr);
   end Is_Based_Literal;

   --------------------
   -- To_Size_String --
   --------------------

   function To_Size_String (Size : String) return String
   is
      Size_Temp : constant String :=
         (if Is_Based_Literal (Size)
          then Ada_Based_Literal_To_C_Style_Hex (Size)
          else Size
         );
   begin
      return Size_Temp;
   end To_Size_String;

   --------------------------------------
   -- Ada_Based_Literal_To_C_Style_Hex --
   --------------------------------------

   function Ada_Based_Literal_To_C_Style_Hex (Value : String) return String
   is
      use Ada.Numerics.Elementary_Functions;

      Integer_Form : constant Long_Integer := Long_Integer'Value (Value);

      --  We need to have at least one characte and we cannot pass
      --  0 to the log function as it raises an exception.
      Size_Of_String_Representation : constant Integer :=
         (if (Integer_Form < 2) then
            1
         else
            Integer (Float'Ceiling (Log (Float (Integer_Form + 1), 16.0))));

      Temp_String : String (1 .. Size_Of_String_Representation + 4);
   begin
      Put
         (To   => Temp_String,
          Item => Integer_Form,
          Base => 16);

      return ("0x" & Temp_String (4 .. Temp_String'Last - 1));
   end Ada_Based_Literal_To_C_Style_Hex;

   --------------------
   -- Validate_Input --
   --------------------

   procedure Validate_Input (Self : in out Spec) is
      use GNAT.Regexp;
      Boot_Mem_Is_Valid : Boolean := False;

      Dec_Number : constant String :=
         "([0-9]+)";

      Dec_Number_And_Unit : constant String :=
         "(" & Dec_Number & "(k|K|m|M))";

      Hex_Number : constant String := "(0x([0-9]|[A-F]|[a-f])+)";

      Address_Pattern : constant String := Hex_Number;

      Size_Pattern : constant String :=
         Hex_Number & "|" & Dec_Number_And_Unit;

      Address_Reg : constant Regexp := Compile (Pattern => Address_Pattern);
      Size_Reg    : constant Regexp := Compile (Pattern => Size_Pattern);

   begin
      for Memory_Region of Self.Memory loop
         if Memory_Region.Name = Self.Boot_Memory then
            --  We found the boot memory.
            Boot_Mem_Is_Valid := True;
         end if;

         --  If the size or the memory are not matching, we raise an exception.
         if not Match (To_String (Memory_Region.Size), Size_Reg) then
            raise Name_Error
               with "Invalid memory size expression : " &
                  To_String (Memory_Region.Size);
         end if;

         if not Match (To_String (Memory_Region.Address), Address_Reg) then
            raise Name_Error
               with "Invalid memory address expression : " &
                  To_String (Memory_Region.Address);
         end if;
      end loop;

      if not Boot_Mem_Is_Valid then
         raise Name_Error
               with "Invalid boot memory : " & To_String (Self.Boot_Memory);
      end if;

   end Validate_Input;

   -----------------------------
   -- Validate_Memory_Regions --
   -----------------------------

   procedure Validate_Memory_Regions (Self : in out Spec) is
   begin
      for Region of Self.Memory loop
         for Memory_Region_To_Check_Against of Self.Memory loop
            --  We dont check the memory region against itself.
            if Memory_Region_To_Check_Against.Name /= Region.Name then
               if Memory_Regions_Overlap
                  (Region,
                   Memory_Region_To_Check_Against)
               then
                  raise Name_Error with "Invalid memory ranges : " &
                     ASCII.LF & Get_Info_String (Region) &
                     ASCII.LF &
                        Get_Info_String (Memory_Region_To_Check_Against);
               end if;
            end if;
         end loop;
      end loop;
   end Validate_Memory_Regions;

   ----------------------------
   -- Memory_Regions_Overlap --
   ----------------------------

   function Memory_Regions_Overlap (Memory_1 : Memory_Region;
                                    Memory_2 : Memory_Region)
                                    return Boolean
   is
      Memory_1_Address : constant Long_Integer :=
         LD_Hex_String_To_Long_Integer (Memory_1.Address);

      Memory_1_Size : constant Long_Integer :=
         LD_Hex_String_To_Long_Integer (Memory_1.Size);

      Memory_2_Address : constant Long_Integer :=
         LD_Hex_String_To_Long_Integer (Memory_2.Address);

      Memory_2_Size : constant Long_Integer :=
         LD_Hex_String_To_Long_Integer (Memory_2.Size);

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

   ----------------------------------
   -- LD_Hex_String_To_Long_Integer --
   ----------------------------------

   function LD_Hex_String_To_Long_Integer (Number : Unbounded_String)
      return Long_Integer
   is
      Nb_Str : constant String := To_String (Number);

      Last_Char : constant Character :=
        Element
            (Source => Number,
             Index  => Length (Number));

      Last_Index  :  Integer := Nb_Str'Last;
      First_Index :  Integer := 3;

      Unit_Multiplier :  Long_Integer := 1;

      Int_Repr :  Long_Integer := 0;
   begin

      -- When we are dealing with an expression, we need to change the
      -- range of the splice in order to convert the string an Ada
      -- based literal.
      case Last_Char is
         when 'k' | 'K' =>
            Unit_Multiplier := 1024;
            Last_Index  := Last_Index - 1;
            First_Index := 1;
         when 'm' | 'M' =>
            Unit_Multiplier := 1024 * 1024;
            Last_Index  := Last_Index - 1;
            First_Index := 1;
         when others => null;
      end case;

      Int_Repr :=
         Long_Integer'Value ("16#" & Nb_Str (First_Index .. Last_Index) & "#");

      return Int_Repr * Unit_Multiplier;
   end LD_Hex_String_To_Long_Integer;

   function Get_Info_String (Region : Memory_Region) return String is
   begin
      return To_String (Region.Name) & " : Size = " & To_String (Region.Size) &
               "  and Address = " & To_String (Region.Address);
   end Get_Info_String;

   -----------------------
   -- Put_Dummy_Handler --
   -----------------------

   procedure Put_Dummy_Handler
      (File : in out Indented_File_Writer;
       Name : String)
   is
   begin
      File.Put_Indented_Line (".weak      "  & Name &"_Handler");
      File.Put_Indented_Line (".thumb_set " & Name & "_Handler,hang");
   end Put_Dummy_Handler;

   -------------------
   -- Put_Interrupt --
   -------------------

   procedure Put_Interrupt
      (File : in out Indented_File_Writer;
       Name : String)
   is
   begin
      File.Put_Indented_Line (".word " & Name & "_Handler " &
         ASCII.HT & "/* "& Name &" */");
   end Put_Interrupt;

end Device;
