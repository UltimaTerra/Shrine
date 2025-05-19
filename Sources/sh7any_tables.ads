with Ada.Finalization;
generic
   -- NOTE TO SELF: Ada like to group 'generic' types together, so we will do that here.
   -- V and M are array-like, used for our tables. 
   -- Note that these Tables are inherited at most 'one level' deep, so we can use a generic type for the tables.
   -- Otherwise, it would get messy to use nested generics like how some languages do.
   type V is private;
   type M is private;

   type Array_V is array (Positive range <>) of V;
   type Array_M is array (Positive range <>) of M;

   VO : Array_V; 
   MO : Array_M;
   -- pragma Assertion_Policy (Assert => CHECK, Precondition => CHECK, Postcondition => IGNORE, Type_Invariant => DISABLE);
package sh7any_tables is 
   procedure SetV (V : in out Array_V; Index : Positive; Value : V);
   procedure SetM (M : in out Array_M; Index : Positive; Value : M);

   pragma Ada_2022;
   pragma Preelaborate (sh7any_tables);
   -- pragma Assertion_Policy (Assert => CHECK, Precondition => CHECK, Postcondition => IGNORE, Type_Invariant => DISABLE);
   type UUID is mod 2**128; 
   type Static_Arena is access all UUID;

   package HashObjPkg is 

      type HashObjTable is new Ada.Finalization.Limited_Controlled with record
         Ref_Count : Integer;
         Name : String(1..128); 
         ID_Key : UUID;
         ID_Value : Array_V;
         Is_Pooled : Boolean;
         Is_Weak : Boolean;
         Arena_IDX : Static_Arena;
      end record;

      type CallTable is new HashObjTable;
      type MessageTable is new HashObjTable;
      type ForeignBufTable is new HashObjTable;
      type RingBufTable is new HashObjTable;

      procedure SortChildren(
         Parent : in out HashObjTable;
         Child : in out HashObjTable;
         Sort_Children : Boolean := True;
         Sort_Parent : Boolean := True); 
      function Create (
         Name : String(1..128);
         ID_Key : UUID;
         ID_Value : Array_V;
         Is_Pooled : Boolean := False;
         Is_Weak : Boolean := False) return HashObjTable;
      function Destroy (
         Object : in out HashObjTable) return Boolean;

      overriding procedure Initialize (Object : in out HashObjTable);
      overriding procedure Finalize (Object : in out HashObjTable);

         --- other 'special tables' private API functions relative to the global package: sh7any_tables.

   -- END HashObjTable;
   -- Use for:  MetaTable and some less stateful tables
   end HashObjPkg;

   package MetaTablePkg is 

      type MetaTable is new HashObjTable with record 
         M_Key: Array_M;
         Locate_UUID : access UUID; 
         -- All the other fields. But since now the M (keys) are generic, we can alternate either with the UUID or the M_Key
         -- or both.
      end record;

      type ShaderTable is new MetaTable;
      type QueueTable is new MetaTable;
      type CoreTable is new MetaTable;

      function Extend_Table (Obj : in out MetaTable; M_Key_Param : Array_M) return MetaTable;
      function Add_Signal (Dummy : Integer) return Integer; -- TODO: Params include a table, and returns a signal (task)
      function Allocate_Hash (Obj : in out MetaTable) return HashObjTable;
      function Destroy (Obj : in out MetaTable) return Boolean;
      function Create (
         M_Key : Array_M;
         Name : String(1..128);
         ID_Key : UUID;
         ID_Value : Array_V;
         Is_Pooled : Boolean := False;
         Is_Weak : Boolean := False) return MetaTable;
      procedure Sort_Children(
         Parent : in out MetaTable;
         Child : in out MetaTable;
         Sort_Children : Boolean := True;
         Sort_Parent : Boolean := True);
      procedure Locate_Relative ( 
         Obj : in out MetaTable;
         Locate_UUID : access UUID;
         ID_Key : UUID);

         --- other 'special tables' private API functions relative to the global package: sh7any_tables.
      overriding procedure Initialize (Object : in out MetaTable);
      overriding procedure Finalize (Object : in out MetaTable);

      -- NOTE: 
      -- RAII for the MetaTable with 2 generic arrays is a tinge tricky
      --  Locate_Relative should have a pointer to 'self' as an UUID but also relative UUID with our Object MetaTable
      -- It might be best to do this api if we want to call the Destroy Function. 
      -- Locate_Relative() -> Sort_Children() -> Destroy() --which, if it deallocates, leads to--> [Ada Runtime Automatically Calls] Finalize()

   end MetaTablePkg; 


 -- END OF TYPE DEFINITIONS: 
 -- Table API =?
 -- We might:
 -- Transform all known tables and refeerences into a tree of parent : children
 -- Collect and list all known tables
 -- Allocate a list of a mix of tables with RAII as our API leans towards
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --




 -- Query API
 -- Things get juicy here: 
 -- We often query by UUID rather then values because relative to all tables in the current applications 'pool'
 -- We also track their relative location with Locate_Relative()
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --
 --


end sh7any_tables;