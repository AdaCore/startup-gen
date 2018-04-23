with GNAT.Strings;      use GNAT.Strings;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

-----------
-- Setup --
-----------

package Setup is

   type Command_Line_Values is record
      Project_File    : aliased String_Access := null;
      Output_Dir      : aliased String_Access := new String'("./");
      Linker_File     : aliased String_Access := new String'("linker.ld");
      Memory_Map_File : aliased String_Access := new String'("memory_map.ld");
   end record;

  --  We verify that the mandatory arguments are correct.
   procedure Get_Arguments (Values : aliased out Command_Line_Values)
      with Post => (Values.Project_File.all /= "")
     or else raise Name_Error
        with "You must pass a file on the command line.";

end Setup;
