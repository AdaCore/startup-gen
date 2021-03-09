------------------------------------------------------------------------------
--                                                                          --
--                               startup-gen                                --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

with GNATCOLL.Projects;     use GNATCOLL.Projects;

with Templates_Parser;

package Device is

   type Spec is tagged private;

   procedure Get_Memory_List_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Get_Interrupt_Vector_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Get_Boot_Memory_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Get_CPU_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Get_User_Tags_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Display (Self : in out Spec);

   --  Checks that the specs are valid, IE:
   --    Validate Inputs (address, sizes, etc...)
   --    No overlapping memory_regions
   --    Boot memory exists
   --    No overlapping interrupts in the interrupt vector.
   --  return False if there is an error
   function  Valid (Self : in out Spec) return Boolean;

   procedure Dump_Linker_Script (Self : in out Spec; Filename : String);

   procedure Dump_Startup_Code (Self : in out Spec; Filename : String);

   procedure Dump_Translate_Table (Self : in out Spec);

   function To_Translate_Table
     (Self : Spec)
      return Templates_Parser.Translate_Table;

private

   type Float_Type is (Hard, Soft);

   function Convert (Str : String) return Float_Type;

   type CPU_Type is record
      Name                 : Unbounded_String;
      Float_Handling       : Float_Type;
      Number_Of_Interrupts : Natural := 0;
      Arch                 : Unbounded_String;
   end record;

   type Memory_Kind is (RAM, ROM);

   type Memory_Region is record
      Name    : Unbounded_String;
      Address : Unbounded_String;
      Size    : Unbounded_String;
      Kind    : Memory_Kind;
   end record;

   package Memory_Region_Vectors is new Ada.Containers.Vectors
      (Positive, Memory_Region);

   use Ada.Containers;
   function Identity_Integer (Key : Integer) return Hash_Type
      is (Hash_Type (Key));

   function Identity_Unbounded_String (Key : Unbounded_String) return Hash_Type
      is (Ada.Strings.Hash (To_String (Key)));

   package Interrupt_Hashed_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type        => Integer,
       Element_Type    => Unbounded_String,
       Hash            => Identity_Integer,
       Equivalent_Keys => "=");

   type Interrupt_Vector is tagged record
      Interrupts  : Interrupt_Hashed_Maps.Map;
      Last_Index  : Integer := -1;
   end record;

   package User_Tags_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Unbounded_String,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   type Spec is tagged record
      Memory            : Memory_Region_Vectors.Vector;
      Boot_Memory       : Unbounded_String;
      Boot_From_ROM     : Boolean;
      CPU               : CPU_Type;
      Interrupts        : Interrupt_Vector;
      Linker_Template   : Unbounded_String;
      Startup_Template  : Unbounded_String;
      Main_Stack_Size   : Unbounded_String;
      Main_Stack_Memory : Unbounded_String;
      User_Tags         : User_Tags_Maps.Map;
   end record;

   --  Private procedures  --

   procedure Add_Interrupt
      (Self  : in out Interrupt_Vector;
       Index : Integer;
       Name  : Unbounded_String);

   --  Used to check if an interrupt has been defined for a given Index.
   function Is_Index_Used
      (Self  : Interrupt_Vector;
       Index : Integer) return Boolean;

   function Get_Name
      (Self  : Interrupt_Vector;
       Index : Integer) return String;

   --  Checks that the input is coherent IE:
   --    Boot memory is a valid memory region.
   --    All memory regions have an address and a size in a relevant format.
   function Valid_Input (Self : in out Spec) return Boolean;

   --  Checks that there are no overlapping memory regions.
   function Valid_Memory_Regions (Self : in out Spec) return Boolean;

   --  Verify that two memory regions are not overlapping each other.
   function Memory_Regions_Overlap (Memory_1 : Memory_Region;
                                    Memory_2 : Memory_Region)
                                    return Boolean;

   --  Return a string of the following form
   --  <region.name> with Size = <region.size> and Address = <region.address>;
   function Get_Info_String (Region : Memory_Region) return String;

   --  Return the default linker template based on the device information
   function Default_Linker_Template (Self : Spec) return String;

   --  Return the default startup template based on the device information
   function Default_Startup_Template (Self : Spec) return String;

end Device;
