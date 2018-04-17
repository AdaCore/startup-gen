with GNATCOLL.Projects; use GNATCOLL.Projects;

package Utils is

   procedure Register_Memory_Map_Attributes with
      Post => (Attribute_Registered ("Mem_Kind", "Memory")
               and Attribute_Registered ("Size", "Memory")
               and Attribute_Registered ("Start", "Memory")
               and Attribute_Registered ("Memories", "Memory")
               );
end Utils;
