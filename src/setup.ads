------------------------------------------------------------------------------
--                                                                          --
--                               startup-gen                                --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Strings;      use GNAT.Strings;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package Setup is

   type Command_Line_Values is tagged record
      Project_File      : aliased String_Access := null;
      Linker_File       : aliased String_Access := null;
      Startup_Code_File : aliased String_Access := null;
      Print_Tags        : aliased Boolean       := False;
   end record;

   --  We verify that the mandatory arguments are correct.
   procedure Get_Arguments (Values : aliased out Command_Line_Values)
      with Post => (Values.Project_File.all /= "")
     or else raise Name_Error
        with "You must pass a file on the command line.";

   procedure Apply_Scenario_Variables (Env : Project_Environment_Access);

   procedure Display (Values : in out Command_Line_Values);

end Setup;
