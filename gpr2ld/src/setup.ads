with GNAT.Strings;      use GNAT.Strings;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

-----------
-- Setup --
-----------

package Setup is

  --  We verify that the mandatory arguments are correct.
   procedure Get_Arguments
     (Config_File : aliased out String_Access;
      Linker_File : aliased out String_Access;
      Output_Dir  : aliased out String_Access)
      with Post => (Config_File.all /= "")
      or else raise Name_Error
        with "You must pass a file on the command line.";

end Setup;
