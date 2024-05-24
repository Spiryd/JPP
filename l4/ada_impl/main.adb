with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Test_Dining_Philosophers is
   
   protected type Fork is
      entry Grab;
      procedure Put_Down;
   private
      Seized : Boolean := False;
   end Fork;
   
   protected body Fork is
      entry Grab when not Seized is
      begin
         Seized := True;
      end Grab;
      procedure Put_Down is
      begin
         Seized := False;
      end Put_Down;
   end Fork;
   
   Life_Span : constant := 3;    -- In his life a philosopher eats 20 times
   
   task type Person (ID : Integer; First, Second : not null access Fork);
   task body Person is
      Dice : Generator;
   begin
      Reset (Dice);
      for Life_Cycle in 1..Life_Span loop
         Put_Line (Integer'Image (ID) & " is thinking");
         delay Duration (Random (Dice) * 0.100);
         Put_Line (Integer'Image (ID) & " is hungry");
         First.Grab;
         Second.Grab;
         Put_Line (Integer'Image (ID) & " is eating");
         delay Duration (Random (Dice) * 0.100);
         Second.Put_Down;
         First.Put_Down;
      end loop;
      Put_Line (Integer'Image (ID) & " is leaving");
   end Person;

   Num_Philosophers : constant := 10; -- Change this to your desired number of philosophers
   Forks : array (1..Num_Philosophers) of aliased Fork; -- Forks for hungry philosophers
   Philosophers : array (1..Num_Philosophers) of access Person;
begin
   for I in 1..Num_Philosophers loop
      Philosophers (I) := new Person (I, Forks (I)'Access, Forks ((I mod Num_Philosophers) + 1)'Access);
   end loop;
   null; -- Nothing to do in the main task, just sit and behold
end Test_Dining_Philosophers;