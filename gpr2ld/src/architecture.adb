with GNATCOLL.VFS; use GNATCOLL.VFS;

package body Architecture is

   -----------------------
   -- Make_Architecture --
   -----------------------

   function Make_Arch_Algorithms
      (Directory : String;
       Name : String)
       return Arch_Algorithms
   is
      Dir_FS : constant Filesystem_String :=
         Filesystem_String (Directory);

      Name_FS : constant Filesystem_String :=
         Filesystem_String (Name);

      Copy_Code_File : constant Virtual_File :=
         Create (Dir_FS) / Name_FS / "copy.S";

      Clear_Code_File : constant Virtual_File :=
         Create (Dir_FS) / Name_FS / "clear.S";

      Temp : Arch_Algorithms;
   begin
      Temp.Clear_Code := Get_File_Content (Clear_Code_File);
      Temp.Copy_Code  := Get_File_Content (Copy_Code_File);
      return Temp;
   end Make_Arch_Algorithms;

end Architecture;
