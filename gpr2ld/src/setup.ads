with GNAT.Strings;      use GNAT.Strings;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

-----------
-- Setup --
-----------

package Setup is

   -------------------
   -- Get_Arguments --
   -------------------
   --  We verify that the mandatory arguments are correct and set correctly

   procedure Get_Arguments
     (Config_File, Linker_File, Output_Dir : aliased out String_Access) with
      Post => (Config_File.all /= "")
      or else raise Name_Error
        with "You must pass a file on the command line.";

end Setup;
