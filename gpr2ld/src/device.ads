pragma Ada_12;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

with File_Writer; use File_Writer;

with Sections;

------------
-- Device --
------------

package Device is

   type Spec is tagged private;

   procedure Get_Memory_List_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Get_Boot_Memory_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

   procedure Get_CPU_From_Project
      (Self         : in out Spec;
       Spec_Project : Project_Type);

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
   type Interrupt is record
      Name : Unbounded_String;
   end record;

   type Float_Type is (Hard, Soft);

   type CPU_Type is record
      Name           : Unbounded_String;
      Float_Handling : Float_Type;
   end record;

   type Memory_Kind is (RAM, ROM, TCM, CCM);

   type Memory_Region is record
      Name    : Unbounded_String;
      Address : Unbounded_String;
      Size    : Unbounded_String;
      Kind    : Memory_Kind;
   end record;

   package Interrupt_Vectors is new Ada.Containers.Vectors
      (Positive, Interrupt);

   package Memory_Region_Vectors is new Ada.Containers.Vectors
      (Positive, Memory_Region);

   type Spec is tagged record
      Memory           : Memory_Region_Vectors.Vector;
      Boot_Memory      : Unbounded_String;
      CPU              : CPU_Type;
      Interrupt_Vector : Interrupt_Vectors.Vector;
      Section_Vector   : Sections.Section_Vectors.Vector;
   end record;

   --  Private procedures  --

   procedure Dump_Header
      (Self : in out Spec;
       File : in out Indented_File_Writer);

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

   --  XXX: Dump the init code for initializing each section (if needed).
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

   --  TODO: Checks that there are no overlapping interrupts.
   procedure Validate_Interrupts (Self : in out Spec);

   --  TODO: For now only works with hexadecimal sizes.
   --  Handle the case of the size with a unit for the size.
   --  Verify that two memory regions are not overlapping each other.
   function Check_Memory_Range (Memory_1 : Memory_Region;
                                Memory_2 : Memory_Region)
                                return Boolean;

   --  Convert a C style hexadecimal string to a Long_Integer.
   function C_Style_Hexa_To_Long_Integer (Number : Unbounded_String)
      return Long_Integer;

   --  Return a string of the following form
   --  <region.name> with Size = <region.size> and Address = <region.address>;
   function Get_Info_String (Region : Memory_Region) return String;

end Device;
