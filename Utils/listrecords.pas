unit ListRecords;

{$mode Delphi}{$H+}

// Needed Second unit for DELPHI Operator overload

interface

uses
  SysUtils;

type

  TOpendFileInfo = record
    Name: String;
    Line, Pos: Integer;
    View: Integer;
    class operator Equal (a, b: TOpendFileInfo) R: Boolean;
  end;

  TFuncInfo = record
    Name: String;
    Info: String;
    FileName: String;
    Line: Integer;
    class operator Equal (a, b: TFuncInfo) R: Boolean;
  end;

  TVarInfo = record
    Name: String;
    FileName: String;
    Line: Integer;
    Pos: Integer;
    class operator Equal (a, b: TVarInfo) R: Boolean;
  end;

implementation
    class operator TOpendFileInfo.Equal (a, b: TOpendFileInfo) R: Boolean;
    begin
      R := (a.Name=b.Name) And (a.Line=b.Line) And (a.Pos=b.Pos) And (a.View=b.View);
    end;
    class operator TFuncInfo.Equal (a, b: TFuncInfo) R: Boolean;
    begin
      R:=(a.Name=b.Name) And (a.Line=b.Line) And (a.FileName=b.FileName);
    end;
    class operator TVarInfo.Equal (a, b: TVarInfo) R: Boolean;
    begin
      R:=(a.Name=b.Name) And (a.Line=b.Line) And (a.Pos=b.Pos) And (a.FileName=b.FileName);
    end;

end.

