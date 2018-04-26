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

   package Unbounded_String_Vectors is new Ada.Containers.Vectors
      (Positive, Unbounded_String);

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

   procedure Dump_Linker_Script (Self : in out Spec; VF : Virtual_File);

   procedure Dump_Memory_Map (Self : in out Spec; VF : Virtual_File);

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
      Name  : Unbounded_String;
      Start : Unbounded_String;
      Size  : Unbounded_String;
      Kind  : Memory_Kind;
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
       Name        : Unbounded_String;
       Permissions : Unbounded_String;
       Start       : Unbounded_String;
       Size        : Unbounded_String);

end Device;
