with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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

   procedure Display (Self : in out Spec);

   --  Checks that the specs are valid, IE:
   --    Validate Inputs (address, sizes, etc...)
   --    No overlapping memory_regions
   --    Boot memory exists
   --    No overlapping interrupts in the interrupt vector.
   --  Throws if there is an error
   procedure Validate (Self : in out Spec);

   procedure Dump_Linker_Script (Self : in out Spec; Filename : String);

   procedure Dump_Startup_Code (Self : in out Spec; Filename : String);

   function To_Translate_Table
     (Self : Spec)
      return Templates_Parser.Translate_Table;

private
   type Float_Type is (Hard, Soft);

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
      Last_Index  : Integer := 0;
   end record;

   type Spec is tagged record
      Memory           : Memory_Region_Vectors.Vector;
      Boot_Memory      : Unbounded_String;
      CPU              : CPU_Type;
      Interrupts       : Interrupt_Vector;
      Linker_Template  : Unbounded_String;
      Startup_Template : Unbounded_String;
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

   --  Translate the size string into a C style hexa notation if needed.
   --  If it is already C style or if it is of the form `192K` we dont
   --  change it and return it untouched.
   function To_Size_String (Size : String) return String;

   --  Translates an Ada based literal string to a C style hexa string.
   function Ada_Based_Literal_To_C_Style_Hex (Value : String) return String;

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

   --  Return the default linker template based on the device information
   function Default_Linker_Template (Self : Spec) return String;

   --  Return the default startup template based on the device information
   function Default_Startup_Template (Self : Spec) return String;

end Device;
