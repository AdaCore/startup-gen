with Ada.Text_IO; use Ada.Text_IO;

package body Device is

   procedure Display (Self : Spec) is
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

   procedure Get_Memory_List_From_Project (Self : in out Spec;
                                        Spec_Project : Project_Type) is
      use Mem_Vect;
      --  Can we group some of those. No we cant, probably.
      Memory_List : constant Attribute_Pkg_List :=
                     Build ("memory", "memories");
      Size_Table : constant Attribute_Pkg_String := Build ("memory", "size");
      Start_Table : constant Attribute_Pkg_String := Build ("memory", "start");

      Kind_Table : constant Attribute_Pkg_String :=
                     Build ("memory", "mem_kind");

   begin
      for Memory of Spec_Project.Attribute_Value (Memory_List).all loop
         declare
            Size : constant String := Spec_Project.
                     Attribute_Value (Size_Table, Index => Memory.all);

            Start : constant String := Spec_Project.
                     Attribute_Value (Start_Table, Index => Memory.all);

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
