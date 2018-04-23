with Ada.Text_IO; use Ada.Text_IO;

package body Device is

   procedure Display (Self : in out Spec) is
   begin
      Put_Line ("CPU: " & To_String (Self.CPU.Name));
      Put_Line ("Float_Handling: " &
                 Float_Type'Image (Self.CPU.Float_Handling));
      for Memory of Self.Memory loop
         Put_Line ("Name : " & To_String (Memory.Name));
         Put_Line ("Start : " & To_String (Memory.Start));
         Put_Line ("Size : " & To_String (Memory.Size));
         Put_Line ("Kind : " & Memory_Kind'Image (Memory.Kind));
      end loop;
   end Display;

   --  Dump Linker Script --
   procedure Dump_Linker_Script (Self : in out Spec; VF : Virtual_File) is
      File : Indented_File_Writer := Make (Handle => Write_File (VF));
   begin
      File.Put_Line ("SEARCH_DIR(.)");

      --  TODO: replace `rom` by starting memory, for now we boot in ROM
      File.Put_Line ("ENTRY(_start_rom);");

      Self.Dump_Sections (File);

      File.Close;
   end Dump_Linker_Script;

   procedure Dump_Sections (Self : in out Spec;
                            File : in out Indented_File_Writer) is
   begin
      File.Put_Line ("SECTIONS");
      File.Put_Line ("{");
      File.Indent;

      --  TODO

      File.Unindent;
      File.Put_Line ("}");
   end Dump_Sections;

   procedure Get_Memory_List_From_Project (Self : in out Spec;
                                        Spec_Project : Project_Type) is
      use Mem_Vect;

      Memory_List : constant Attribute_Pkg_List :=
                     Build ("Memory_map", "memories");
      Size_Table : constant Attribute_Pkg_String :=
                     Build ("Memory_map", "Size");
      Address_Table : constant Attribute_Pkg_String :=
                     Build ("Memory_map", "Start");
      Kind_Table : constant Attribute_Pkg_String :=
                     Build ("Memory_map", "mem_kind");

   begin
      for Memory of Spec_Project.Attribute_Value (Memory_List).all loop
         declare
            Size : constant String := Spec_Project.
                     Attribute_Value (Size_Table, Index => Memory.all);

            Start : constant String := Spec_Project.
                     Attribute_Value (Address_Table, Index => Memory.all);

            Kind : constant String := Spec_Project.
                     Attribute_Value (Kind_Table, Index => Memory.all);

            Memory_Unit : constant Memory_Type :=
                                  (Name => To_Unbounded_String (Memory.all),
                                   Start => To_Unbounded_String (Start),
                                   Size => To_Unbounded_String (Size),
                                   Kind => Memory_Kind'Value (Kind));
         begin
            Self.Memory := Self.Memory & Memory_Unit;
         end;
      end loop;

   end Get_Memory_List_From_Project;

   procedure Get_CPU_From_Project (Self : in out Spec;
                                Spec_Project : Project_Type) is

      Name : constant String :=  Spec_Project.
                           Attribute_Value (Build ("cpu", "name"));

      Float_Handling : constant String :=  Spec_Project.
                           Attribute_Value (Build ("cpu", "float_handling"));
   begin
      Self.CPU := (To_Unbounded_String (Name),
                   Float_Type'Value (Float_Handling));

   end Get_CPU_From_Project;

end Device;
