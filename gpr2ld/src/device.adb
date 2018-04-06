with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNAT.Strings; use GNAT.Strings;
with Ada.Text_IO; use Ada.Text_IO;

package body Device is

   procedure Set_Memory_List (Self : in out Spec; Path : String)
   is
      Tree : Project_Tree;
      Root : Project_Type;
      VFS : constant Virtual_File := Create_From_Base
                                       (Filesystem_String (Path));
      Memory_List : constant Attribute_Pkg_List := Build
                                       ("memory",
                                        "memory_list");
      Size_Table : constant Attribute_Pkg_String := Build
                                       ("memory",
                                        "size");
      Start_Table : constant Attribute_Pkg_String := Build
                                       ("memory",
                                        "start");

   begin
      GNATCOLL.Projects.Load (Tree,
         Root_Project_Path => VFS,
         Packages_To_Check => All_Packs);

      Root := Tree.Root_Project;

      for Memory of Root.Attribute_Value (Memory_List).all loop
         declare
            Size : constant String := Root.Attribute_Value (Size_Table,
                                                   Index => Memory.all);

            Start : constant String := Root.Attribute_Value (Start_Table,
                                                   Index => Memory.all);
           --   Memory_Unit : Memory_Type :=
           --                         (Name => To_Unbounded_String ("TestRAM"),
           --                          Start => 16#0#,
           --                          Size => 16#100#,
           --                          Kind => RAM);

         begin
            Put_Line ("Mem  : " & Memory.all);
            Put_Line ("Size : " & Size);
            Put_Line ("Start: " & Start);
         end;
      end loop;

      Self.CPU := (Name => To_Unbounded_String (""),
                            Float_Handling => Soft);
   end Set_Memory_List;

   procedure Register_Memory_Map_Attributes
   is
      Error1 : constant String := Register_New_Attribute
                              ("Memory_List",
                              "Memory",
                              Is_List => True);

      Error2 : constant String := Register_New_Attribute
                           ("Size",
                           "Memory",
                           Indexed => True);

      Error3 : constant String := Register_New_Attribute
                           ("Start",
                           "Memory",
                           Indexed => True);

   begin
      if Error1 /= "" or else Error2 /= "" or else Error3 /= "" then
         raise Program_Error;
      end if;
   end Register_Memory_Map_Attributes;

end Device;
