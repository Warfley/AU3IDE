unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type
  TTokenType = (tkComment, tkIdentifier, tkFunction, tkSymbol, tkNumber, tkSpace,
    tkString, tkUnknown, tkVar, tkUndefined);

  PHashInfo = ^THashInfo;

  THashInfo = record
    Key: ansistring;
    Kind: TTokenType;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    IdentifierB: TToggleBox;
    FunctionB: TToggleBox;
    Label10: TLabel;
    DocB: TToggleBox;
    DocBC: TColorButton;
    DocBG: TToggleBox;
    DocFB: TToggleBox;
    DocFC: TColorButton;
    DocI: TToggleBox;
    DocTC: TColorButton;
    DocU: TToggleBox;
    SymbolB: TToggleBox;
    NumberB: TToggleBox;
    SpaceB: TToggleBox;
    StringB: TToggleBox;
    VariableB: TToggleBox;
    OtherB: TToggleBox;
    IdentifierBC: TColorButton;
    FunctionBC: TColorButton;
    SymbolBC: TColorButton;
    NumberBC: TColorButton;
    SpaceBC: TColorButton;
    StringBC: TColorButton;
    VariableBC: TColorButton;
    OtherBC: TColorButton;
    IdentifierBG: TToggleBox;
    FunctionBG: TToggleBox;
    SymbolBG: TToggleBox;
    NumberBG: TToggleBox;
    SpaceBG: TToggleBox;
    StringBG: TToggleBox;
    VariableBG: TToggleBox;
    OtherBG: TToggleBox;
    IdentifierFB: TToggleBox;
    FunctionFB: TToggleBox;
    SymbolFB: TToggleBox;
    NumberFB: TToggleBox;
    SpaceFB: TToggleBox;
    StringFB: TToggleBox;
    VariableFB: TToggleBox;
    OtherFB: TToggleBox;
    IdentifierFC: TColorButton;
    FunctionFC: TColorButton;
    SymbolFC: TColorButton;
    NumberFC: TColorButton;
    SpaceFC: TColorButton;
    StringFC: TColorButton;
    VariableFC: TColorButton;
    OtherFC: TColorButton;
    IdentifierI: TToggleBox;
    FunctionI: TToggleBox;
    SymbolI: TToggleBox;
    NumberI: TToggleBox;
    SpaceI: TToggleBox;
    StringI: TToggleBox;
    VariableI: TToggleBox;
    OtherI: TToggleBox;
    CommentTC: TColorButton;
    CommentFC: TColorButton;
    CommentBC: TColorButton;
    ComboBox1: TComboBox;
    IdentifierTC: TColorButton;
    FunctionTC: TColorButton;
    SymbolTC: TColorButton;
    NumberTC: TColorButton;
    SpaceTC: TColorButton;
    StringTC: TColorButton;
    VariableTC: TColorButton;
    OtherTC: TColorButton;
    IdentifierU: TToggleBox;
    FunctionU: TToggleBox;
    SymbolU: TToggleBox;
    NumberU: TToggleBox;
    SpaceU: TToggleBox;
    StringU: TToggleBox;
    VariableU: TToggleBox;
    OtherU: TToggleBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    CommentB: TToggleBox;
    CommentFB: TToggleBox;
    CommentBG: TToggleBox;
    CommentI: TToggleBox;
    CommentU: TToggleBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    FHashList: array[0..255] of TList;
    procedure LoadHLTable(Path: string);
    procedure SaveHLTable(Path: string);
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

const
  CaseInsensitive = True;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 255 do
    FHashList[i] := TList.Create;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  i, x: integer;
begin
  OpenDialog1.Filter := 'Keywordlist | Keywords.lst';
  if OpenDialog1.Execute then
  begin
    LoadHLTable(OpenDialog1.FileName);
    ListBox1.Clear;
    for i := 0 to 255 do
      for x := 0 to FHashList[i].Count - 1 do
        ListBox1.Items.Add(PHashInfo(FHashList[i][x])^.Key);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);

var
  i, x: integer;
begin
  for x := 0 to 255 do
    for i := 0 to FHashList[x].Count - 1 do
      Dispose(PHashInfo(FHashList[x][i]));
end;

procedure TForm1.Button7Click(Sender: TObject);
type
  TFontInfo = packed record
    FontCol: TColor;
    Big, Italics, Underline, Frame: boolean;
    FrameColor: TColor;
    Background: boolean;
    BackColor: TColor;
  end;

var
  tmp: TFontInfo;
  fs: TFileStream;
begin
  SaveDialog1.Filter := 'Highlight Config | Colors.cnf';
  if SaveDialog1.Execute then
  begin
    fs := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      // Comment
      tmp.FontCol := CommentTC.ButtonColor;
      tmp.Big := CommentB.Checked;
      tmp.Italics := CommentI.Checked;
      tmp.Underline := CommentU.Checked;
      tmp.Frame := CommentFB.Checked;
      tmp.FrameColor := CommentFC.ButtonColor;
      tmp.Background := CommentBG.Checked;
      tmp.BackColor := CommentBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
      // Identifier
      tmp.FontCol := IdentifierTC.ButtonColor;
      tmp.Big := IdentifierB.Checked;
      tmp.Italics := IdentifierI.Checked;
      tmp.Underline := IdentifierU.Checked;
      tmp.Frame := IdentifierFB.Checked;
      tmp.FrameColor := IdentifierFC.ButtonColor;
      tmp.Background := IdentifierBG.Checked;
      tmp.BackColor := IdentifierBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
      // Function
      tmp.FontCol := FunctionTC.ButtonColor;
      tmp.Big := FunctionB.Checked;
      tmp.Italics := FunctionI.Checked;
      tmp.Underline := FunctionU.Checked;
      tmp.Frame := FunctionFB.Checked;
      tmp.FrameColor := FunctionFC.ButtonColor;
      tmp.Background := FunctionBG.Checked;
      tmp.BackColor := FunctionBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
      // Symbol
      tmp.FontCol := SymbolTC.ButtonColor;
      tmp.Big := SymbolB.Checked;
      tmp.Italics := SymbolI.Checked;
      tmp.Underline := SymbolU.Checked;
      tmp.Frame := SymbolFB.Checked;
      tmp.FrameColor := SymbolFC.ButtonColor;
      tmp.Background := SymbolBG.Checked;
      tmp.BackColor := SymbolBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
      // Number
      tmp.FontCol := NumberTC.ButtonColor;
      tmp.Big := NumberB.Checked;
      tmp.Italics := NumberI.Checked;
      tmp.Underline := NumberU.Checked;
      tmp.Frame := NumberFB.Checked;
      tmp.FrameColor := NumberFC.ButtonColor;
      tmp.Background := NumberBG.Checked;
      tmp.BackColor := NumberBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
      // Space
      tmp.FontCol := SpaceTC.ButtonColor;
      tmp.Big := SpaceB.Checked;
      tmp.Italics := SpaceI.Checked;
      tmp.Underline := SpaceU.Checked;
      tmp.Frame := SpaceFB.Checked;
      tmp.FrameColor := SpaceFC.ButtonColor;
      tmp.Background := SpaceBG.Checked;
      tmp.BackColor := SpaceBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
      // String
      tmp.FontCol := StringTC.ButtonColor;
      tmp.Big := StringB.Checked;
      tmp.Italics := StringI.Checked;
      tmp.Underline := StringU.Checked;
      tmp.Frame := StringFB.Checked;
      tmp.FrameColor := StringFC.ButtonColor;
      tmp.Background := StringBG.Checked;
      tmp.BackColor := StringBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
      // Variable
      tmp.FontCol := VariableTC.ButtonColor;
      tmp.Big := VariableB.Checked;
      tmp.Italics := VariableI.Checked;
      tmp.Underline := VariableU.Checked;
      tmp.Frame := VariableFB.Checked;
      tmp.FrameColor := VariableFC.ButtonColor;
      tmp.Background := VariableBG.Checked;
      tmp.BackColor := VariableBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
      // Doc
      tmp.FontCol := DocTC.ButtonColor;
      tmp.Big := DocB.Checked;
      tmp.Italics := DocI.Checked;
      tmp.Underline := DocU.Checked;
      tmp.Frame := DocFB.Checked;
      tmp.FrameColor := DocFC.ButtonColor;
      tmp.Background := DocBG.Checked;
      tmp.BackColor := DocBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
      // Other
      tmp.FontCol := OtherTC.ButtonColor;
      tmp.Big := OtherB.Checked;
      tmp.Italics := OtherI.Checked;
      tmp.Underline := OtherU.Checked;
      tmp.Frame := OtherFB.Checked;
      tmp.FrameColor := OtherFC.ButtonColor;
      tmp.Background := OtherBG.Checked;
      tmp.BackColor := OtherBC.ButtonColor;
      fs.Write(tmp, SizeOf(tmp));
    finally
      fs.Free;
    end;
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
type
  TFontInfo = packed record
    FontCol: TColor;
    Big, Italics, Underline, Frame: boolean;
    FrameColor: TColor;
    Background: boolean;
    BackColor: TColor;
  end;

var
  tmp: TFontInfo;
  fs: TFileStream;
begin
  OpenDialog1.Filter := 'Highlight Config | Colors.cnf';
  if OpenDialog1.Execute then
  begin
    fs := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      // Comment
      fs.Read(tmp, SizeOf(tmp));
      CommentTC.ButtonColor := tmp.FontCol;
      CommentB.Checked := tmp.Big;
      CommentI.Checked := tmp.Italics;
      CommentU.Checked := tmp.Underline;
      CommentFB.Checked := tmp.Frame;
      CommentFC.ButtonColor := tmp.FrameColor;
      CommentBG.Checked := tmp.Background;
      CommentBC.ButtonColor := tmp.BackColor;
      // Identifier
      fs.Read(tmp, SizeOf(tmp));
      IdentifierTC.ButtonColor := tmp.FontCol;
      IdentifierB.Checked := tmp.Big;
      IdentifierI.Checked := tmp.Italics;
      IdentifierU.Checked := tmp.Underline;
      IdentifierFB.Checked := tmp.Frame;
      IdentifierFC.ButtonColor := tmp.FrameColor;
      IdentifierBG.Checked := tmp.Background;
      IdentifierBC.ButtonColor := tmp.BackColor;
      // Function
      fs.Read(tmp, SizeOf(tmp));
      FunctionTC.ButtonColor := tmp.FontCol;
      FunctionB.Checked := tmp.Big;
      FunctionI.Checked := tmp.Italics;
      FunctionU.Checked := tmp.Underline;
      FunctionFB.Checked := tmp.Frame;
      FunctionFC.ButtonColor := tmp.FrameColor;
      FunctionBG.Checked := tmp.Background;
      FunctionBC.ButtonColor := tmp.BackColor;
      // Symbol
      fs.Read(tmp, SizeOf(tmp));
      SymbolTC.ButtonColor := tmp.FontCol;
      SymbolB.Checked := tmp.Big;
      SymbolI.Checked := tmp.Italics;
      SymbolU.Checked := tmp.Underline;
      SymbolFB.Checked := tmp.Frame;
      SymbolFC.ButtonColor := tmp.FrameColor;
      SymbolBG.Checked := tmp.Background;
      SymbolBC.ButtonColor := tmp.BackColor;
      // Number
      fs.Read(tmp, SizeOf(tmp));
      NumberTC.ButtonColor := tmp.FontCol;
      NumberB.Checked := tmp.Big;
      NumberI.Checked := tmp.Italics;
      NumberU.Checked := tmp.Underline;
      NumberFB.Checked := tmp.Frame;
      NumberFC.ButtonColor := tmp.FrameColor;
      NumberBG.Checked := tmp.Background;
      NumberBC.ButtonColor := tmp.BackColor;
      // Space
      fs.Read(tmp, SizeOf(tmp));
      SpaceTC.ButtonColor := tmp.FontCol;
      SpaceB.Checked := tmp.Big;
      SpaceI.Checked := tmp.Italics;
      SpaceU.Checked := tmp.Underline;
      SpaceFB.Checked := tmp.Frame;
      SpaceFC.ButtonColor := tmp.FrameColor;
      SpaceBG.Checked := tmp.Background;
      SpaceBC.ButtonColor := tmp.BackColor;
      // String
      fs.Read(tmp, SizeOf(tmp));
      StringTC.ButtonColor := tmp.FontCol;
      StringB.Checked := tmp.Big;
      StringI.Checked := tmp.Italics;
      StringU.Checked := tmp.Underline;
      StringFB.Checked := tmp.Frame;
      StringFC.ButtonColor := tmp.FrameColor;
      StringBG.Checked := tmp.Background;
      StringBC.ButtonColor := tmp.BackColor;
      // Variable
      fs.Read(tmp, SizeOf(tmp));
      VariableTC.ButtonColor := tmp.FontCol;
      VariableB.Checked := tmp.Big;
      VariableI.Checked := tmp.Italics;
      VariableU.Checked := tmp.Underline;
      VariableFB.Checked := tmp.Frame;
      VariableFC.ButtonColor := tmp.FrameColor;
      VariableBG.Checked := tmp.Background;
      VariableBC.ButtonColor := tmp.BackColor;
      // Doc
      fs.Read(tmp, SizeOf(tmp));
      DocTC.ButtonColor := tmp.FontCol;
      DocB.Checked := tmp.Big;
      DocI.Checked := tmp.Italics;
      DocU.Checked := tmp.Underline;
      DocFB.Checked := tmp.Frame;
      DocFC.ButtonColor := tmp.FrameColor;
      DocBG.Checked := tmp.Background;
      DocBC.ButtonColor := tmp.BackColor;
      // Other
      fs.Read(tmp, SizeOf(tmp));
      OtherTC.ButtonColor := tmp.FontCol;
      OtherB.Checked := tmp.Big;
      OtherI.Checked := tmp.Italics;
      OtherU.Checked := tmp.Underline;
      OtherFB.Checked := tmp.Frame;
      OtherFC.ButtonColor := tmp.FrameColor;
      OtherBG.Checked := tmp.Background;
      OtherBC.ButtonColor := tmp.BackColor;
    finally
      fs.Free;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  x, i: integer;
begin
  for x := 0 to 255 do
    for i := 0 to FHashList[x].Count - 1 do
      if PHashInfo(FHashList[x][i])^.Key = ListBox1.Items[ListBox1.ItemIndex] then
      begin
        Dispose(PHashInfo(FHashList[x][i]));
        FHashList[x].Delete(i);
        ListBox1.Items.Delete(ListBox1.ItemIndex);
      end;
end;

procedure TForm1.Button3Click(Sender: TObject);

  function GetHash(toHash: PChar; out Len: integer): byte;
  begin
    Result := 0;
    Len := 0;
    while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
    begin
      if CaseInsensitive and (toHash^ in ['A'..'Z']) then
        Result := (Result + Ord(toHash^) - Ord('A') + Ord('a')) mod 256
      else
        Result := (Result + Ord(toHash^)) mod 256;
      Inc(ToHash);
      Inc(Len);
    end;
  end;

var
  x: byte;
  Dummy: integer;
  tmp: PHashInfo;
begin
  x := GetHash(PChar(Edit1.Text), Dummy);
  new(tmp);
  tmp^.Key := Edit1.Text;
  tmp^.Kind := TTokenType(ComboBox1.ItemIndex);
  FHashList[x].Add(tmp);
  ListBox1.Items.Add(Edit1.Text);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  SaveDialog1.Filter := 'Keywordlist | Keywords.lst';
  if SaveDialog1.Execute then
    SaveHLTable(SaveDialog1.FileName);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button2Click(nil);
  Button3Click(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  Button6Click(nil);
  for i := 0 to 255 do
    FHashList[i].Free;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  x, i: integer;
begin
  if ListBox1.ItemIndex >= 0 then
    for x := 0 to 255 do
      for i := 0 to FHashList[x].Count - 1 do
        if PHashInfo(FHashList[x][i])^.Key = ListBox1.Items[ListBox1.ItemIndex] then
        begin
          Edit1.Text := PHashInfo(FHashList[x][i])^.Key;
          ComboBox1.ItemIndex := Ord(PHashInfo(FHashList[x][i])^.Kind);
        end;
end;

procedure TForm1.LoadHLTable(Path: string);
var
  fs: TFileStream;
  tmp: PHashInfo;
  i, x, n, a: integer;
begin
  Button6Click(nil);
  fs := TFileStream.Create(Path, fmOpenRead);
  try
    for x := 0 to 255 do
    begin
      fs.Read(n, SizeOf(n));
      for i := 0 to n - 1 do
      begin
        new(tmp);
        fs.Read(tmp^.Kind, SizeOf(tmp^.Kind));
        fs.Read(a, SizeOf(a));
        SetLength(tmp^.Key, a);
        fs.Read(tmp^.Key[1], a);
        FHashList[x].Add(tmp);
      end;
    end;
  finally
    fs.Free;
  end;
end;

procedure TForm1.SaveHlTable(Path: string);
var
  fs: TFileStream;
  tmp: THashInfo;
  i, x, a: integer;

begin
  fs := TFileStream.Create(Path, fmCreate);
  try
    for x := 0 to 255 do
    begin
      fs.Write(FHashList[x].Count, SizeOf(integer));
      for i := 0 to FHashList[x].Count - 1 do
      begin
        tmp := PHashInfo(FHashList[x][i])^;
        fs.Write(tmp.Kind, SizeOf(tmp.Kind));
        a := Length(tmp.Key);
        fs.Write(a, SizeOf(a));
        fs.Write(tmp.Key[1], a);
      end;
    end;
  finally
    fs.Free;
  end;

end;

end.
