with Ada.Text_IO; use Ada.Text_IO;

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
      function TUS (Str : String) return Unbounded_String
         renames To_Unbounded_String;

      TEXT : constant Section :=
          Make_Section (Boot_Memory        => Self.Boot_Memory,
                        Name               => TUS ("text"),
                        Reloc_Memory       => Self.Boot_Memory
                       );

      RODATA : constant Section :=
          Make_Section (Boot_Memory  => Self.Boot_Memory,
                        Name         => TUS ("rodata"),
                        Reloc_Memory => Self.Boot_Memory
                       );

       BSS : constant Section :=
         Make_Section (Boot_Memory    => Self.Boot_Memory,
                       Name           => TUS ("bss"),
                       Reloc_Memory   => TUS ("RAM"),
                       Force_Init     => True
                       );

      DATA : constant Section :=
         Make_Section (Boot_Memory    => Self.Boot_Memory,
                       Name           => TUS ("data"),
                       Reloc_Memory   => TUS ("RAM")
                       );

       CCMDATA : constant Section :=
         Make_Section (Boot_Memory    => Self.Boot_Memory,
                       Name           => TUS ("ccmdata"),
                       Reloc_Memory   => TUS ("RAM")
                     );
       use Sections.Sect_Vect;
   begin
        Self.Section_Vector := Self.Section_Vector &
          TEXT & RODATA & BSS & DATA & CCMDATA;
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

      --  TODO: replace `rom` by starting memory, for now we boot in ROM
      File.Put_Line ("ENTRY(_start_rom);");

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

      --  TODO

      File.Unindent;
      File.Put_Line ("}");
   end Dump_Sections;

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
