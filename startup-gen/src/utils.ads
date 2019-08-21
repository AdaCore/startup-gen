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

package Utils is

   Prj_Package_Name : constant String := "Device_Configuration";

   procedure Register_Memory_Map_Attributes;

   Exit_Exc : exception;

   procedure Warning (Msg : String);
   procedure Error (Msg : String);
   procedure Fatal_Error (Msg : String)
     with No_Return;

end Utils;
