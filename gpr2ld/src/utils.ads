with GNATCOLL.Projects; use GNATCOLL.Projects;

package Utils is

   procedure Register_Memory_Map_Attributes with
      Post => (Attribute_Registered ("Types", "Memory")
               and Attribute_Registered ("Size", "Memory")
               and Attribute_Registered ("Start", "Memory")
               );
end Utils;
