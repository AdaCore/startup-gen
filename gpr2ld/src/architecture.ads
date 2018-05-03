with Startup;               use Startup;

package Architecture is

   type Arch_Algorithms is record
      Copy_Code  : Algorithm;
      Clear_Code : Algorithm;
   end record;

   function Make_Arch_Algorithms
      (Directory : String;
       Name : String)
       return Arch_Algorithms;

end Architecture;
