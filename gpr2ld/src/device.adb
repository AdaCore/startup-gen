with Ada.Text_IO; use Ada.Text_IO;

with Startup; use Startup;

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

            Start : constant String :=
              Spec_Project.Attribute_Value
                (Address_Table, Index => Memory.all);

            Kind : constant String :=
              Spec_Project.Attribute_Value (Kind_Table, Index => Memory.all);

            Memory_Unit : constant Memory_Region :=
              (Name  => To_Unbounded_String (Memory.all),
               Start => To_Unbounded_String (Start),
               Size  => To_Unbounded_String (Size),
               Kind  => Memory_Kind'Value (Kind));
         begin
            Self.Memory := Self.Memory & Memory_Unit;
         end;
      end loop;

   end Get_Memory_List_From_Project;

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

      Name : constant String :=
        Spec_Project.Attribute_Value (Build ("cpu", "name"));

      Float_Handling : constant String :=
        Spec_Project.Attribute_Value (Build ("cpu", "float_handling"));
   begin
      Self.CPU :=
        (To_Unbounded_String (Name),
         Float_Type'Value (Float_Handling));

   end Get_CPU_From_Project;

   -----------------------
   -- Generate_Sections --
   -----------------------

   procedure Generate_Sections (Self : in out Spec) is
      use Sections;
      use Unbounded_String_Vectors;
      function TUS (Str : String) return Unbounded_String
         renames To_Unbounded_String;

      --  In the long term we want to be able to specify
      --  the sections that corresponds to the target architecture.
      --  For now and for ARM processors, it will do.

      TEXT : constant Section :=
         Make_Section
            (Boot_Memory        => Self.Boot_Memory,
             Name               => TUS ("text"),
             Reloc_Memory       => Self.Boot_Memory,
             Additional_Content => Unbounded_String_Vectors.Empty_Vector &
                                    TUS ("KEEP (*(.vectors))")
            );

      RODATA : constant Section :=
         Make_Section
            (Boot_Memory  => Self.Boot_Memory,
             Name         => TUS ("rodata"),
             Reloc_Memory => Self.Boot_Memory
                       );

       BSS : constant Section :=
         Make_Section
            (Boot_Memory  => Self.Boot_Memory,
             Name         => TUS ("bss"),
             Reloc_Memory => TUS ("RAM"),
             Force_Init   => True,
             Load         => False,
             Init_Code    => Clear_Memory_Code
                       );

      DATA : constant Section :=
         Make_Section
            (Boot_Memory  => Self.Boot_Memory,
             Name         => TUS ("data"),
             Reloc_Memory => TUS ("RAM"),
             Init_Code    => Copy_Memory_Code
                       );

       CCMDATA : constant Section :=
         Make_Section
            (Boot_Memory  => Self.Boot_Memory,
             Name         => TUS ("ccmdata"),
             Reloc_Memory => TUS ("RAM"),
             Init_Code    => Copy_Memory_Code
                     );

       use Sections.Section_Vectors;
   begin
        Self.Section_Vector := Self.Section_Vector &
          TEXT & RODATA & DATA & BSS & CCMDATA;
   end Generate_Sections;

   -------------
   -- Display --
   -------------

   procedure Display (Self : in out Spec) is
   begin
      Put_Line ("CPU: " & To_String (Self.CPU.Name));
      Put_Line
        ("Float_Handling: " & Float_Type'Image (Self.CPU.Float_Handling));
      for Memory of Self.Memory loop
         Put_Line ("Name : " & To_String (Memory.Name));
         Put_Line ("Start : " & To_String (Memory.Start));
         Put_Line ("Size : " & To_String (Memory.Size));
         Put_Line ("Kind : " & Memory_Kind'Image (Memory.Kind));
      end loop;
   end Display;

   ------------------------
   -- Dump_Linker_Script --
   ------------------------

   procedure Dump_Linker_Script (Self : in out Spec; VF : Virtual_File) is
      File : Indented_File_Writer := Make (Handle => Write_File (VF));
   begin
      File.Put_Line ("SEARCH_DIR(.)");
      File.New_Line;
      File.Put_Line ("__DYNAMIC = 0;");
      File.New_Line;
      File.Put_Line ("_DEFAULT_STACK_SIZE = 4 * 1024;");

      File.New_Line;
      File.Put_Line ("ENTRY(_start_" & Self.Boot_Memory & ");");
      File.New_Line;

      Self.Dump_Sections (File);

      File.Close;
   end Dump_Linker_Script;

   ---------------------
   -- Dump_Memory_Map --
   ---------------------

   procedure Dump_Memory_Map
      (Self : in out Spec;
       VF   : Virtual_File) is
       File : Indented_File_Writer := Make
            (Handle => Write_File (VF));
   begin
      File.Put_Line ("MEMORY");
      File.Put_Line ("{");
      File.Indent;

      for Memory of Self.Memory loop
         declare
            Permissions : constant Unbounded_String :=
                     To_Unbounded_String (if Memory.Kind = ROM
                                             then "(rx)"
                                             else "(rwx)");
         begin
            Dump_Memory (File, Memory.Name, Permissions,
                               Memory.Start, Memory.Size);
         end;
      end loop;

      File.Unindent;
      File.Put_Line ("}");

      File.Close;
   end Dump_Memory_Map;

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
         (if (Self.Boot_Memory /= Section.Reloc_Memory)
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

      --  XXX: Hardcoded stack for the BSS.
      if Section.Name = "bss" then
        File.New_Line;

         File.Put_Indented_Line ("__stack_start = .;");
         File.Put_Indented_Line (". += DEFINED (__stack_size) ?" &
               " __stack_size : _DEFAULT_STACK_SIZE;");
         File.Put_Indented_Line (". = ALIGN(0x8);");
         File.Put_Indented_Line ("__stack_end = .;");

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
       Name        : Unbounded_String;
       Permissions : Unbounded_String;
       Start       : Unbounded_String;
       Size        : Unbounded_String)
   is
   begin
      File.Put_Indented_Line (Name & " " & Permissions & " : ORIGIN = " &
                              Start & ", LENGTH = " & Size);
   end Dump_Memory;

end Device;
