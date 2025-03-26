with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure Main is
   type Expr_Kind is (Add, Sub, Mul, Div, Number, Parens);
   type Expr;
   type Expr_Access is access Expr;

   type Expr is record
      Kind  : Expr_Kind;
      Left  : Expr_Access := null;
      Right : Expr_Access := null;
      Value : Integer := 0;
   end record;

   Input : Unbounded_String;
   Index : Natural := 1;

   function Current_Char return Character is
   begin
      if Index > Length(Input) then
         return ASCII.NUL;
      else
         return Element(Input, Index);
      end if;
   end;

   procedure Advance is
   begin
      Index := Index + 1;
   end;

   procedure Skip_Whitespace is
   begin
      while Current_Char in ' ' | ASCII.HT loop
         Advance;
      end loop;
   end;

   function Parse_Number return Expr_Access is
      Num : Integer := 0;
   begin
      while Current_Char in '0' .. '9' loop
         Num := Num * 10 + Character'Pos(Current_Char) - Character'Pos('0');
         Advance;
      end loop;
      return new Expr'(Kind => Number, Left => null, Right => null, Value => Num);
   end;

   function Parse_Factor return Expr_Access;

   function Parse_Term return Expr_Access is
      LHS : Expr_Access := Parse_Factor;
   begin
      loop
         Skip_Whitespace;
         case Current_Char is
            when '*' =>
               Advance;
               LHS := new Expr'(Kind => Mul, Left => LHS, Right => Parse_Factor, Value => 0);
            when '/' =>
               Advance;
               LHS := new Expr'(Kind => Div, Left => LHS, Right => Parse_Factor, Value => 0);
            when others =>
               exit;
         end case;
      end loop;
      return LHS;
   end;

   function Parse_Expr return Expr_Access is
      LHS : Expr_Access := Parse_Term;
   begin
      loop
         Skip_Whitespace;
         case Current_Char is
            when '+' =>
               Advance;
               LHS := new Expr'(Kind => Add, Left => LHS, Right => Parse_Term, Value => 0);
            when '-' =>
               Advance;
               LHS := new Expr'(Kind => Sub, Left => LHS, Right => Parse_Term, Value => 0);
            when others =>
               exit;
         end case;
      end loop;
      return LHS;
   end;

   function Parse_Factor return Expr_Access is
   begin
      Skip_Whitespace;
      if Current_Char = '(' then
         Advance;
         declare
            SubExpr : Expr_Access := Parse_Expr;
         begin
            if Current_Char = ')' then
               Advance;
            else
               Put_Line("Error: expected ')'");
            end if;
            return SubExpr;
         end;
      elsif Current_Char in '0' .. '9' then
         return Parse_Number;
      else
         Put_Line("Error: Unexpected character '" & String'(1 => Current_Char) & "'");
         return new Expr'(Kind => Number, Left => null, Right => null, Value => 0);
      end if;
   end;

   function Evaluate(E : Expr_Access) return Integer is
   begin
      case E.Kind is
         when Number =>
            return E.Value;
         when Add =>
            return Evaluate(E.Left) + Evaluate(E.Right);
         when Sub =>
            return Evaluate(E.Left) - Evaluate(E.Right);
         when Mul =>
            return Evaluate(E.Left) * Evaluate(E.Right);
         when Div =>
            declare
               R : Integer := Evaluate(E.Right);
            begin
               if R = 0 then
                  Put_Line("Error: Division by zero.");
                  return 0;
               else
                  return Evaluate(E.Left) / R;
               end if;
            end;
         when Parens =>
            return Evaluate(E.Left);
      end case;
   end;

begin
   Put_Line("ADA Calculator — Type 'q' to quit.");
   loop
      Put(">> ");
      Input := To_Unbounded_String(Get_Line);
      exit when To_String(Input) = "q";

      Index := 1;

      declare
         AST : Expr_Access := Parse_Expr;
         Result : Integer := Evaluate(AST);
      begin
         Put_Line("= " & Integer'Image(Result));
      end;
   end loop;
end Main;

