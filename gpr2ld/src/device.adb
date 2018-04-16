with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNAT.Strings; use GNAT.Strings;
with Ada.Text_IO; use Ada.Text_IO;

package body Device is

   procedure Dump (Self : Spec) is
   begin
      for Memory of Self.Memory loop
         Put_Line ("Name : " & To_String (Memory.Name));
         Put_Line ("Start : " & To_String (Memory.Start));
         Put_Line ("Size : " & To_String (Memory.Size));
         Put_Line ("Kind : " & Memory_Kind'Image (Memory.Kind));
      end loop;
   end Dump;

   procedure Set_Memory_List (Self : in out Spec; Path : String) is
      use Device.Mem_Vect;

      Tree : Project_Tree;
      Root : Project_Type;
      VFS : constant Virtual_File := Create_From_Base
                                       (Filesystem_String (Path));

      --  TODO: Can we group some of those
      Memory_List : constant Attribute_Pkg_List := Build ("memory", "types");
      Size_Table : constant Attribute_Pkg_String := Build ("memory", "size");
      Start_Table : constant Attribute_Pkg_String := Build ("memory", "start");

   begin
      GNATCOLL.Projects.Load (Tree,
         Root_Project_Path => VFS,
         Packages_To_Check => All_Packs);

      Root := Tree.Root_Project;

      --  TODO dont hardcode the memory type.
      for Memory of Root.Attribute_Value (Memory_List).all loop
         declare
            Size : constant String := Root.Attribute_Value (Size_Table,
                                                   Index => Memory.all);

            Start : constant String := Root.Attribute_Value (Start_Table,
                                                   Index => Memory.all);
            Memory_Unit : constant Memory_Type :=
                                  (Name => To_Unbounded_String ("TestRAM"),
                                   Start => To_Unbounded_String (Start),
                                   Size => To_Unbounded_String (Size),
                                   Kind => RAM);
         begin
            Self.Memory := Self.Memory & Memory_Unit;
         end;
      end loop;

      Self.CPU := (Name => To_Unbounded_String (""),
                            Float_Handling => Soft);
   end Set_Memory_List;

end Device;
