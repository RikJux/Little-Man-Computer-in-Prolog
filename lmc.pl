
%checks that all values are within 0 and 999
valid_range_of_values([]).

valid_range_of_values([X]) :- !,
    integer(X),
    X >=0,
    X =< 999.

valid_range_of_values([X | L]) :- valid_range_of_values([X]),
    valid_range_of_values(L).

%Value in accumulator must be within 0 and 99
valid_acc(Acc) :- valid_range_of_values([Acc]).

%PC value must be within 0 and 99
valid_pc(Pc) :- integer(Pc),
    Pc >= 0,
    Pc =< 99.
%Mem length must be 100 and must contain valid values only
valid_mem(Mem) :- length(Mem, 100),
    valid_range_of_values(Mem).

%Checks that input has valid values
valid_in(In) :- valid_range_of_values(In).

%Checks that output has valid values
valid_out(Out) :- valid_range_of_values(Out).

%Checks that flag is correct
valid_flag(Flag) :- Flag = flag,
    !.

valid_flag(Flag) :- Flag = noflag,
    !.
%Function that determines presence of flags
decide_flag(Result, noflag) :- valid_range_of_values([Result]),
    !.

decide_flag(_, flag) :- !.

%Definition of a state
state(Acc, Pc, Mem, In, Out, Flag) :- valid_acc(Acc),
    valid_pc(Pc),
    valid_mem(Mem),
    valid_in(In),
    valid_out(Out),
    valid_flag(Flag).

%Definition of halted_state
halted_state(Acc, Pc, Mem, In, Out, Flag) :- state(Acc, Pc, Mem, In, Out, Flag),
    nth0(Pc, Mem, Instruction, _),
    Instruction =< 99.

%Checks the validity of an initial state
initial(state(0, 0, Mem, In, [], noflag)) :- valid_mem(Mem),
    valid_in(In).

%Checks the validity of a final state
final(state(Acc, Pc, Mem, In, Out, Flag)) :-
    halted_state(Acc, Pc, Mem, In, Out, Flag).

%1XX
one_instruction(state(Acc, Pc, Mem, In, Out, _),
                state(NewAcc, NewPc, Mem, In, Out, NewFlag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction >= 100,
    Instruction =< 199,
    TemporaryPointer is Instruction - 100,
    nth0(TemporaryPointer, Mem, Addendum, _),
    Result is Acc + Addendum,
    NewAcc is Result mod 1000,
    decide_flag(Result, NewFlag),
    ProvPc is Pc + 1,
    NewPc is ProvPc mod 1000.

%2XX
one_instruction(state(Acc, Pc, Mem, In, Out, _),
                state(NewAcc, NewPc, Mem, In, Out, NewFlag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction >= 200,
    Instruction =< 299,
    TemporaryPointer is Instruction - 200,
    nth0(TemporaryPointer, Mem, Subtrahend, _),
    Result is Acc - Subtrahend,
    NewAcc is Result mod 1000,
    decide_flag(Result, NewFlag),
    ProvPc is Pc + 1,
    NewPc is ProvPc mod 1000.

%3XX
one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(Acc, NewPc, NewMem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction >= 300,
    Instruction =< 399,
    Destination is Instruction - 300,
    nth0(Destination, Mem, _, R),
    nth0(Destination, NewMem, Acc, R),
    ProvPc is Pc + 1,
    NewPc is ProvPc mod 1000.

%5XX
one_instruction(state(_, Pc, Mem, In, Out, Flag),
                state(NewAcc, NewPc, Mem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction >= 500,
    Instruction =< 599,
    Source is Instruction - 500,
    nth0(Source, Mem, NewAcc, _),
    ProvPc is Pc + 1,
    NewPc is ProvPc mod 1000.

%6XX
one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(Acc, NewPc, Mem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction >= 600,
    Instruction =< 699,
    NewPc is Instruction - 600.

%7XX
one_instruction(state(0, Pc, Mem, In, Out, noflag),
                state(0, NewPc, Mem, In, Out, noflag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction >= 700,
    Instruction =< 799,
    NewPc is Instruction - 700.

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(Acc, NewPc, Mem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction >= 700,
    Instruction =< 799,
    ProvPc is Pc+1,
    NewPc is ProvPc mod 1000.

%8XX
one_instruction(state(Acc, Pc, Mem, In, Out, noflag),
                state(Acc, NewPc, Mem, In, Out, noflag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction >= 800,
    Instruction =< 899,
    NewPc is Instruction - 800.

one_instruction(state(Acc, Pc, Mem, In, Out, flag),
                state(Acc, NewPc, Mem, In, Out, flag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction >= 800,
    Instruction =< 899,
    ProvPc is Pc + 1,
    NewPc is ProvPc mod 1000.

%901
one_instruction(state(_, Pc, Mem, [NewAcc | In], Out, Flag),
                state(NewAcc, NewPc, Mem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction = 901,
    ProvPc is Pc + 1,
    NewPc is ProvPc mod 1000.

%902
one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(Acc, NewPc, Mem, In, NewOut, Flag)) :-
    nth0(Pc, Mem, Instruction, _),
    Instruction = 902,
    ProvPc is Pc + 1,
    NewPc is ProvPc mod 1000,
    append(Out, [Acc], NewOut).

execution_loop(State, Out) :- initial(State),
    accept(State, Out).

%Given the initial state, produces an output
accept(State, Out) :-
    final(State),
    arg(5, State, Out).

accept(State, Out) :- one_instruction(State, NewState),
    accept(NewState, Out).

%Caps all letters
to_upper_case([], []).
to_upper_case([X | Q], [X | S]) :- X < 97,
    !,
    to_upper_case(Q, S).
to_upper_case([X | Q], [X | S]) :- X > 122,
    !,
    to_upper_case(Q, S).
to_upper_case([X | Q], [Y | S]) :- X =< 122,
    X >= 97,
    !,
    Y is X - 32,
    to_upper_case(Q, S).

%Separates '/' from text comment
separate_comments([], []).
separate_comments(X, Y) :-
    append(['/', '/'], Other, X),
    append([' ', '/', '/', ' '], NewOther, Y),
    separate_comments(Other, NewOther).
separate_comments(X, Y) :-
    append([Char], Other, X),
    append([Char], NewOther, Y),
    char_type(Char, _),
    separate_comments(Other, NewOther).

%Separates '\n' from text comment
separate_escapes([], []).
separate_escapes(X, Y) :-
    append(['\n'], Other, X),
    append([' ', '\n', ' '], NewOther, Y),
    separate_escapes(Other, NewOther).
separate_escapes(X, Y) :-
    append([Char], Other, X),
    append([Char], NewOther, Y),
    char_type(Char, _),
    separate_escapes(Other, NewOther).

number_list_into_char_list([], []).
number_list_into_char_list([D | N], [H | C]) :-
    char_code(H, D),
    number_list_into_char_list(N, C).

%no_comment/2 deletes comments
no_comment([], []).
no_comment([H | Other], [H | NewOther]) :-
           H \= "//",
           no_comment(Other, NewOther).

no_comment(["//" | Other], NewOther) :-
    nihilite(Other, NewOther).

%nihilite/2 used by no_comment
nihilite([], []).
nihilite([H | Other], NewOther) :-
    H \= "\n",
    nihilite(Other, NewOther).
nihilite(["\n" | Other], ["\n" | NewOther]) :-
    no_comment(Other, NewOther).

%elaborates input file in order to upload it into memory
lmc_load(Filename, MemFin) :-
    open(Filename, read, AssemblyFile),
    read_string(AssemblyFile, _, AssemblyString),
    close(AssemblyFile),
    preparate_input(AssemblyString, Is),
    acceptFile(Is, new_line, _, _, _, Mem),
    split(Mem, NewLbl, NewIst, NewArgs),
    unification(NewLbl, NewArgs, NewArgs2),
    to_machine_code(NewIst, NewNewIst),
    create_mem(NewNewIst, NewArgs2, MemFin).

%preparate_input/2 generates memory ignoring comments
preparate_input(Oldmem, MemWithoutComments):-
    string_codes(Oldmem, Charmem),
    to_upper_case(Charmem, Uppermem),
    number_list_into_char_list(Uppermem, MemWithChar),
    append(MemWithChar, [' ', '\n'], MemWithSpaces),
    separate_comments(MemWithSpaces, MemSeparateComments),
    separate_escapes(MemSeparateComments, MemSeparateEscapes),
    split_string(MemSeparateEscapes, ' ', ' ', MemSeparated),
    no_comment(MemSeparated, MemWithoutComments).

%list_of_digits/1 checks that the list is a digits-only list 
list_of_digits([]).
list_of_digits([N | L]) :- char_type(N, digit),
    list_of_digits(L).

%alnum_underscore_list/1 checks that the list is a alphanumerical list,
%undescore accepted too
alnum_underscore_list([]).
alnum_underscore_list([X | L]) :- char_type(X, alnum),
    alnum_underscore_list(L).
alnum_underscore_list([X | L]) :- char_code('_', X),
    alnum_underscore_list(L).

%Definitions of instructions
dat(X) :- X = "DAT".

ist_arg(X) :- X = "ADD".
ist_arg(X) :- X = "SUB".
ist_arg(X) :- X = "STA".
ist_arg(X) :- X = "LDA".
ist_arg(X) :- X = "BRA".
ist_arg(X) :- X = "BRZ".
ist_arg(X) :- X = "BRP".

ist_no_arg(X) :- X = "INP".
ist_no_arg(X) :- X = "OUT".
ist_no_arg(X) :- X = "HLT".

%num_arg/1 checks that arguments are lists of numbers
num_arg(X) :- string_codes(X, Xcodes),
    list_of_digits(Xcodes).

%valid_label/1 checks that the label is valid
valid_label(X) :- X \= "ADD",
    X \= "SUB",
    X \= "STA",
    X \= "LDA",
    X \= "BRA",
    X \= "BRZ",
    X \= "BRP",
    X \= "INP",
    X \= "OUT",
    X \= "HLT",
    X \= "DAT",
    check_first(X),
    string_codes(X, Xcodes),
    alnum_underscore_list(Xcodes), !.

%label_arg/1 checks that an argument label is valid
label_arg(X) :- valid_label(X).

%check_first/1 checks that the first char of a label is
%not a number
check_first(Label):-
    get_string_code(1, Label, Code),
    char_code(First, Code),
    char_type(First, digit), !,
    fail.
check_first(_).

%Definition of the initial state
initial_state(new_line, [], [], [], []).

%Definition of a final state
final_state(new_line, X, Y, Z, Mem) :- append(X, ["|"], NewX),
    append(NewX, Y, W),
    append(W, ["|"], NewW),
    append(NewW, Z, Mem).

%acceptFile/6 from the initial state, applies delta function
%to produce the final state
acceptFile([I | Is], S, Lbl, Ist, Args, Mem) :-
    delta(S, Lbl, Ist, Args, I, N, NewLbl, NewIst, NewArgs),
    acceptFile(Is, N, NewLbl, NewIst, NewArgs, Mem).

acceptFile([], Q, X, Y, Z, Mem) :- final_state(Q, X, Y, Z, Mem),
    is_list(X),
    is_list(Y),
    is_list(Z),
    is_list(Mem).

%split/4 divide la lista 1 prodotta dall'accept file nelle liste di
%Label, Istruzioni e Argomenti
split(Mem, Lbl, Ist, Args):-
    take_till_div(Mem, Lbl, NewMem),
    take_till_div(NewMem, Ist, Args).

take_till_div([], _, []).
take_till_div([L|Resto], [], Resto):-
    L = "|".
take_till_div([L|List], [L|Sublist], Resto):-
    L \= "|",
    take_till_div(List, Sublist, Resto).

%unification/3 gives to each argument the corresponding numerical value
unification(_, [], []).
unification(LblList, [Args|ArgList], [0|Final]):- Args = "",
    unification(LblList, ArgList, Final).
unification(LblList, [Args|ArgList], [Valore|Final]):- valid_label(Args),
    unification(LblList, ArgList, Final),
    search_Lbl(Args, LblList, Valore).
unification(LblList, [Args|ArgList], [Args | Final]):- num_arg(Args),
    unification(LblList, ArgList, Final).

search_Lbl(Args, LblList, Valore):-nth0(Valore, LblList, Args).

delta(new_line, Lbl, Ist, Args, "\n",
      new_line, Lbl, Ist, Args).

delta(new_line, Lbl, Ist, Args, I,
      found_label, NewLbl, Ist, Args) :- valid_label(I),
      append(Lbl, [I], NewLbl).
delta(new_line, Lbl, Ist, Args, I,
      found_ist_arg, NewLbl, NewIst, Args) :- ist_arg(I),
      append(Ist, [I], NewIst),
      append(Lbl, [""], NewLbl).
delta(new_line, Lbl, Ist, Args, I,
      found_ist_no_arg, NewLbl, NewIst, NewArgs) :- ist_no_arg(I),
      append(Ist, [I], NewIst),
      append(Lbl, [""], NewLbl),
      append(Args, [""], NewArgs).
delta(new_line, Lbl, Ist, Args, D,
      found_dat, NewLbl, NewIst, Args) :- dat(D),
      append(Ist, ["DAT"], NewIst),
      append(Lbl, [""], NewLbl).

delta(found_label, Lbl, Ist, Args, I,
      found_ist_arg, Lbl, NewIst, Args) :- ist_arg(I),
      append(Ist, [I], NewIst).
delta(found_label, Lbl, Ist, Args, I,
      found_ist_no_arg, Lbl, NewIst, NewArgs) :- ist_no_arg(I),
      append(Ist, [I], NewIst),
      append(Args, [""], NewArgs).
delta(found_label, Lbl, Ist, Args, D,
      found_dat, Lbl, NewIst, Args) :- dat(D),
      append(Ist, ["DAT"], NewIst).

delta(found_ist_no_arg, Lbl, Ist, Args, "\n",
      new_line, Lbl, Ist, Args).

delta(found_ist_arg, Lbl, Ist, Args, I,
      found_arg, Lbl, Ist, NewArgs) :- label_arg(I),
      append(Args, [I], NewArgs).
delta(found_ist_arg, Lbl, Ist, Args, N,
      found_arg, Lbl, Ist, NewArgs) :- num_arg(N),
      number_codes(NCode, N),
      append(Args, [NCode], NewArgs).

delta(found_dat, Lbl, Ist, Args, "\n",
      new_line, Lbl, Ist, NewArgs) :-
      append(Args, ["0"], NewArgs).
delta(found_dat, Lbl, Ist, Args, I,
      found_arg, Lbl, Ist, NewArgs) :- label_arg(I),
      append(Args, [I], NewArgs).
delta(found_dat, Lbl, Ist, Args, N,
      found_arg, Lbl, Ist, NewArgs) :- num_arg(N),
      number_codes(NCode, N),
      append(Args, [NCode], NewArgs).

delta(found_arg, Lbl, Ist, Args, "\n",
      new_line, Lbl, Ist, Args).

machine_code_of("DAT", 0).
machine_code_of("ADD", 100).
machine_code_of("SUB", 200).
machine_code_of("STA", 300).
machine_code_of("LDA", 500).
machine_code_of("BRA", 600).
machine_code_of("BRZ", 700).
machine_code_of("BRP", 800).
machine_code_of("INP", 901).
machine_code_of("OUT", 902).
machine_code_of("HLT", 0).

%to_machine_code/2 converts instructions into their machine code
to_machine_code([], []).
to_machine_code([I | Ist], [CodeI | CodeIst]) :-
    machine_code_of(I, CodeI),
    to_machine_code(Ist, CodeIst).

%n_zeros/2 creates a list of n zeros, to append to memory so that
%it has 100 elements
n_zeros(0, []).
n_zeros(N, [0 | ZList]) :- M is N-1,
    n_zeros(M, ZList).

create_mem_prov([], [], []).
create_mem_prov([CodeI | CodeIst], [CodeA | CodeArgs], [ElMem | MemProv]) :-
    ElMem is CodeI + CodeA,
    create_mem_prov(CodeIst, CodeArgs, MemProv).

%create_mem/3 generates the memory
create_mem(CodeIst, CodeArgs, Mem) :-
    create_mem_prov(CodeIst, CodeArgs, MemProv),
    length(MemProv, M),
    N is 100-M,
    n_zeros(N, ZList),
    append(MemProv, ZList, Mem).

lmc_run(Filename, Input, Output):-
    lmc_load(Filename, MemFin),
    execution_loop(state(0, 0, MemFin, Input, [], noflag) , Output).
















