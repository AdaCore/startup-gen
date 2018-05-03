pragma Ada_12;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNATCOLL.Projects;     use GNATCOLL.Projects;

with File_Writer;           use File_Writer;

with Sections;
with Architecture;          use Architecture;

------------
-- Device --
------------

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

   procedure Setup_Known_Architectures
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Set_CPU_Architecture_Sample_Code
      (Self : in out Spec);

   procedure Generate_Sections (Self : in out Spec);

   procedure Display (Self : in out Spec);

   --  Checks that the specs are valid, IE:
   --    Validate Inputs (address, sizes, etc...)
   --    No overlapping memory_regions
   --    Boot memory exists
   --    No overlapping interrupts in the interrupt vector.
   --  Throws if there is an error
   procedure Validate (Self : in out Spec);

   procedure Dump_Linker_Script (Self : in out Spec; VF : Virtual_File);

   procedure Dump_Memory_Map (Self : in out Spec; VF : Virtual_File);

   procedure Dump_Startup_Code (Self : in out Spec; VF : Virtual_File);

private
   type Float_Type is (Hard, Soft);

   type CPU_Type is record
      Name           : Unbounded_String;
      Float_Handling : Float_Type;
      Arch           : Arch_Algorithms;
   end record;

   type Memory_Kind is (RAM, ROM, TCM, CCM);

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

   package Architecture_Hashed_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type        => Unbounded_String,
       Element_Type    => Arch_Algorithms,
       Hash            => Identity_Unbounded_String,
       Equivalent_Keys => "=");

   type Interrupt_Vector is tagged record
      Interrupts  : Interrupt_Hashed_Maps.Map;
      Last_Index  : Integer := 0;
   end record;

   type Spec is tagged record
      Memory           : Memory_Region_Vectors.Vector;
      Boot_Memory      : Unbounded_String;
      CPU              : CPU_Type;
      Interrupts       : Interrupt_Vector;
      Architectures    : Architecture_Hashed_Maps.Map;
      Section_Vector   : Sections.Section_Vectors.Vector;
   end record;

   --  Private procedures  --

   procedure Add_Interrupt
      (Self  : in out Interrupt_Vector;
       Index : Integer;
       Name  : Unbounded_String);

   --  Used to check if an interrupt has been defined for a given Index.
   function Is_Index_Used
      (Self  : in out Interrupt_Vector;
       Index : Integer) return Boolean;

   function Get_Name
      (Self  : in out Interrupt_Vector;
       Index : Integer) return String;

   --  Returns the last (bigger) index mapped to an interrupt.
   --  function Last_Index (Self  : in out Interrupt_Vector) return Integer;

   procedure Dump_Header
      (File : in out Indented_File_Writer);

   --  Dump all the current memory sections to the file.
   procedure Dump_Sections
      (Self : in out Spec;
       File : in out Indented_File_Writer);

   --  Dump a single section to the file.
   procedure Dump_Section
      (Self    : in out Spec;
       File    : in out Indented_File_Writer;
       Section : in out Sections.Section);

   --  Dump a single line representing a memory region to the file.
   procedure Dump_Memory
       (File        : in out Indented_File_Writer;
        Memory      : Memory_Region;
        Permissions : Unbounded_String);

   --  XXX: Dump a typical ARM interrupt vector used for ZFP.
   procedure Dump_Interrupt_Vector
      (Self : in out Spec;
       File : in out Indented_File_Writer);

   procedure Dump_Sections_Init_Code
      (Self : in out Spec;
       File : in out Indented_File_Writer);

   --  Translate the size string into a C style hexa notation if needed.
   --  If it is already C style or if it is of the form `192K` we dont
   --  change it and return it untouched.
   function To_Size_String (Size : String) return String;

   --  Translates an Ada based literal string to a C style hexa string.
   function Ada_Based_Literal_To_C_Style_Hex (Size : String) return String;

   --  Returns True if the String passed
   --  in parameter is an Ada based literal.
   function Is_Based_Literal (Number : String) return Boolean;

   --  Checks that the input is coherent IE:
   --    Boot memory is a valid memory region.
   --    All memory regions have an address and a size in a relevant format.
   procedure Validate_Input (Self : in out Spec);

   --  Checks that there are no overlapping memory regions.
   procedure Validate_Memory_Regions (Self : in out Spec);

   --  Verify that two memory regions are not overlapping each other.
   function Memory_Regions_Overlap (Memory_1 : Memory_Region;
                                    Memory_2 : Memory_Region)
                                    return Boolean;

   --  Convert a ld hexadecimal string to a Long_Integer.
   function LD_Hex_String_To_Long_Integer (Number : Unbounded_String)
      return Long_Integer;

   --  Return a string of the following form
   --  <region.name> with Size = <region.size> and Address = <region.address>;
   function Get_Info_String (Region : Memory_Region) return String;

end Device;
