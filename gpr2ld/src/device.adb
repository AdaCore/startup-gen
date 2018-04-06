package body Device is

   procedure Set_Memory_List (This : in out Spec; Path : String)
   is
      Tree : Project_Tree;
      Root : Project_Type;
      VFS : Virtual_File := Create_From_Base (Filesystem_String (Path));
      Memory_List : Attribute_Pkg_List := Build ("memory", "memory_list");
      Size_Table : Attribute_Pkg_String := Build ("memory", "size");
      Start_Table : Attribute_Pkg_String := Build ("memory", "start");

   begin
      GNATCOLL.Projects.Load (Tree,
         Root_Project_Path => VFS,
         Packages_To_Check => All_Packs);

      Root := Tree.Root_Project;

      for Memory of Root.Attribute_Value (Memory_List).all loop
         declare
            Size : String := Root.Attribute_Value (Size_Table,
                                                   Index => Memory.all);

            Start : String := Root.Attribute_Value (Start_Table,
                                                   Index => Memory.all);
            Memory_Unit : Memory_Type := (
                                       Name => To_Unbounded_String ("TestRAM"),
                                       Start => 16#0#,
                                       Size => 16#100#,
                                       Kind => RAM);
         begin
            Put_Line ("Mem  : " & Memory.all);
            Put_Line ("Size : " & Size);
            Put_Line ("Start: " & Start);
         end;
      end loop;

   end Set_Memory_List;

   procedure Register_Memory_Map_Attributes
   is
      Error1 : String := Register_New_Attribute
                              ("Memory_List",
                              "Memory",
                              Is_List => True);

      Error2 : String := Register_New_Attribute
                           ("Size",
                           "Memory",
                           Indexed => True);

      Error3 : String := Register_New_Attribute
                           ("Start",
                           "Memory",
                           Indexed => True);

   begin
      null;
   end Register_Memory_Map_Attributes;

end Device;
