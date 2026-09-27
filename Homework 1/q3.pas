program q3;
type course_kind = (REG, LAB, SEM);
type Course = record
    id : integer;
    course_name : string;
    case kind : course_kind of
        REG : (weekly_hours: integer);
        LAB : (manager_name, location: string);
        SEM : (start, end_time: string);
    end;

type course_list = array[1000..1100] of Course;
procedure add (var course_list: course_list);
var kind: course_kind;
course_name: string;
var id: integer;
begin
    ReadLn(kind);
    ReadLn(id);
    ReadLn(course_name);
    course_list[id].kind := kind;
    course_list[id].id := id;
    course_list[id].course_name := course_name;
    if kind = REG then ReadLn(course_list[id].weekly_hours);
    if kind = LAB then 
    begin
        ReadLn(course_list[id].manager_name);
        ReadLn(course_list[id].location);
    end;
    if kind = SEM then 
    begin
        ReadLn(course_list[id].start);
        ReadLn(course_list[id].end_time);
    end;
end;

procedure print_course(var course_list: course_list); 
var id:integer;
var kind:course_kind;
begin
ReadLn(id);
kind := course_list[id].kind;
WriteLn(kind);
WriteLn(id);
WriteLn(course_list[id].course_name);
case kind of
    REG: WriteLn(course_list[id].weekly_hours);
    LAB:
    begin
        WriteLn(course_list[id].manager_name);
        WriteLn(course_list[id].location);
    end;
    SEM:
    begin
        WriteLn(course_list[id].start);
        WriteLn(course_list[id].end_time);
    end;
end;
end;

var command: string;
var courses_list: course_list;
begin
    ReadLn(command);
    while command <> 'END' do
    begin
        case command of
            'ADD': add(courses_list);
            'PRINT': print_course(courses_list);
        end;
        ReadLn(command);
    end;


end.

