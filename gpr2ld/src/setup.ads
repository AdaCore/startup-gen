with GNAT.Strings;      use GNAT.Strings;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with GNATCOLL.Projects; use GNATCOLL.Projects;

-----------
-- Setup --
-----------

package Setup is

   type Command_Line_Values is tagged record
      Project_File      : aliased String_Access := null;
      Linker_File       : aliased String_Access := null;
      Startup_Code_File : aliased String_Access := null;
   end record;

   --  We verify that the mandatory arguments are correct.
   procedure Get_Arguments (Values : aliased out Command_Line_Values)
      with Post => (Values.Project_File.all /= "")
     or else raise Name_Error
        with "You must pass a file on the command line.";

   procedure Apply_Scenario_Variables (Env : Project_Environment_Access);

   procedure Display (Values : in out Command_Line_Values);

end Setup;
