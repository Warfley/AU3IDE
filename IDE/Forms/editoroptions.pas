unit EditorOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ColorBox, ComCtrls, Editor, strutils, SynGutterBase, SynEdit,
  SynGutterChanges, au3Highlighter, SynEditTypes, au3Types, GraphUtil;

type

  { TEditorConf }

  TEditorConf = class(TForm)
    AddFuncButton: TButton;
    DeleteKeywordButton: TButton;
    BGColorBtn: TButton;
    Button1: TButton;
    Button2: TButton;
    AddKeywordButton: TButton;
    DeleteFuncButton: TButton;
    FuncNameEdit: TEdit;
    KeyTypeBox: TComboBox;
    EditorFontButton: TButton;
    CommentB: TToggleBox;
    CommentBC: TColorButton;
    CommentBG: TToggleBox;
    CommentFB: TToggleBox;
    CommentFC: TColorButton;
    CommentI: TToggleBox;
    CommentTC: TColorButton;
    CommentU: TToggleBox;
    DocB: TToggleBox;
    DocBC: TColorButton;
    DocBG: TToggleBox;
    DocFB: TToggleBox;
    DocFC: TColorButton;
    DocI: TToggleBox;
    DocTC: TColorButton;
    DocU: TToggleBox;
    FontDialog1: TFontDialog;
    FunctionB: TToggleBox;
    FunctionBC: TColorButton;
    FunctionBG: TToggleBox;
    FunctionFB: TToggleBox;
    FunctionFC: TColorButton;
    FunctionI: TToggleBox;
    FunctionTC: TColorButton;
    FunctionU: TToggleBox;
    IdentifierB: TToggleBox;
    IdentifierBC: TColorButton;
    IdentifierBG: TToggleBox;
    IdentifierFB: TToggleBox;
    IdentifierFC: TColorButton;
    IdentifierI: TToggleBox;
    IdentifierTC: TColorButton;
    IdentifierU: TToggleBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    KeyEdit: TLabeledEdit;
    KeyWordView: TListView;
    Label29: TLabel;
    Label30: TLabel;
    FuncBox: TListBox;
    FuncInfoMemo: TMemo;
    Label31: TLabel;
    ModalPanel: TPanel;
    FillPanel: TPanel;
    ConfigPageControl: TPageControl;
    NumberB: TToggleBox;
    NumberBC: TColorButton;
    NumberBG: TToggleBox;
    NumberFB: TToggleBox;
    NumberFC: TColorButton;
    NumberI: TToggleBox;
    NumberTC: TColorButton;
    NumberU: TToggleBox;
    OtherB: TToggleBox;
    OtherBC: TColorButton;
    OtherBG: TToggleBox;
    OtherFB: TToggleBox;
    OtherFC: TColorButton;
    OtherI: TToggleBox;
    OtherTC: TColorButton;
    OtherU: TToggleBox;
    TextColorButton: TButton;
    TextColorPicklist: TColorBox;
    TooltipColorButton: TButton;
    TooltipForeColorButton: TButton;
    TooltipColorPicklist: TColorBox;
    SpaceB: TToggleBox;
    SpaceBC: TColorButton;
    SpaceBG: TToggleBox;
    SpaceFB: TToggleBox;
    SpaceFC: TColorButton;
    SpaceI: TToggleBox;
    SpaceTC: TColorButton;
    SpaceU: TToggleBox;
    StringB: TToggleBox;
    StringBC: TColorButton;
    StringBG: TToggleBox;
    StringFB: TToggleBox;
    StringFC: TColorButton;
    StringI: TToggleBox;
    StringTC: TColorButton;
    StringU: TToggleBox;
    SymbolB: TToggleBox;
    SymbolBC: TColorButton;
    SymbolBG: TToggleBox;
    SymbolFB: TToggleBox;
    SymbolFC: TColorButton;
    SymbolI: TToggleBox;
    SymbolTC: TColorButton;
    SymbolU: TToggleBox;
    TabLenEdit: TEdit;
    Label10: TLabel;
    ScrollPastEOLBox: TCheckBox;
    CaretAlwaysVisibleBox: TCheckBox;
    EditorColorButton: TButton;
    BGColorPicklist: TColorBox;
    EditorFrame1: TEditorFrame;
    GutterColorButton: TButton;
    EditorColorPicklist: TColorBox;
    ColorDialog1: TColorDialog;
    GutterForeColorButton: TButton;
    GutterColorPicklist: TColorBox;
    EditedColorButton: TButton;
    Label8: TLabel;
    Label9: TLabel;
    SavedColorButton: TButton;
    GutterForeColorPicklist: TColorBox;
    EditedColorPicklist: TColorBox;
    SelectedColorButton: TButton;
    SelectedForeColorButton: TButton;
    SavedColorPicklist: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    SelectedColorPicklist: TColorBox;
    SelectedForeColorPicklist: TColorBox;
    CESideBox: TToggleBox;
    GeneralOptionsTab: TTabSheet;
    HighlightOptionsTab: TTabSheet;
    KeywordOptionsTab: TTabSheet;
    FunctionOptionTab: TTabSheet;
    TooltipForeColorPicklist: TColorBox;
    VariableB: TToggleBox;
    VariableBC: TColorButton;
    VariableBG: TToggleBox;
    VariableFB: TToggleBox;
    VariableFC: TColorButton;
    VariableI: TToggleBox;
    VariableTC: TColorButton;
    VariableU: TToggleBox;
    procedure AddFuncButtonClick(Sender: TObject);
    procedure AddKeywordButtonClick(Sender: TObject);
    procedure BGColorBtnClick(Sender: TObject);
    procedure BGColorPicklistChange(Sender: TObject);
    procedure CaretAlwaysVisibleBoxChange(Sender: TObject);
    procedure CESideBoxChange(Sender: TObject);
    procedure CommentBChange(Sender: TObject);
    procedure CommentBGChange(Sender: TObject);
    procedure CommentFBChange(Sender: TObject);
    procedure CommentFCColorChanged(Sender: TObject);
    procedure CommentIChange(Sender: TObject);
    procedure CommentTCColorChanged(Sender: TObject);
    procedure CommentUChange(Sender: TObject);
    procedure DeleteFuncButtonClick(Sender: TObject);
    procedure DeleteKeywordButtonClick(Sender: TObject);
    procedure DocBChange(Sender: TObject);
    procedure DocBGChange(Sender: TObject);
    procedure DocFBChange(Sender: TObject);
    procedure DocFCColorChanged(Sender: TObject);
    procedure DocIChange(Sender: TObject);
    procedure DocTCClick(Sender: TObject);
    procedure DocUChange(Sender: TObject);
    procedure EditedColorButtonClick(Sender: TObject);
    procedure EditedColorPicklistChange(Sender: TObject);
    procedure EditedColorPicklistGetColors(Sender: TCustomColorBox;
      Items: TStrings);
    procedure EditorColorButtonClick(Sender: TObject);
    procedure EditorColorPicklistChange(Sender: TObject);
    procedure EditorFontButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FuncBoxClick(Sender: TObject);
    procedure FuncNameEditChange(Sender: TObject);
    procedure FunctionBChange(Sender: TObject);
    procedure FunctionBGChange(Sender: TObject);
    procedure FunctionFBChange(Sender: TObject);
    procedure FunctionFCColorChanged(Sender: TObject);
    procedure FunctionIChange(Sender: TObject);
    procedure FunctionTCColorChanged(Sender: TObject);
    procedure FunctionUChange(Sender: TObject);
    procedure GutterColorButtonClick(Sender: TObject);
    procedure GutterColorPicklistChange(Sender: TObject);
    procedure GutterForeColorButtonClick(Sender: TObject);
    procedure GutterForeColorPicklistChange(Sender: TObject);
    procedure IdentifierBChange(Sender: TObject);
    procedure IdentifierBGChange(Sender: TObject);
    procedure IdentifierFBChange(Sender: TObject);
    procedure IdentifierFCColorChanged(Sender: TObject);
    procedure IdentifierIChange(Sender: TObject);
    procedure IdentifierTCClick(Sender: TObject);
    procedure IdentifierTCColorChanged(Sender: TObject);
    procedure IdentifierUChange(Sender: TObject);
    procedure KeyEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure KeyWordViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure NumberBChange(Sender: TObject);
    procedure NumberBGChange(Sender: TObject);
    procedure NumberFBChange(Sender: TObject);
    procedure NumberFCColorChanged(Sender: TObject);
    procedure NumberIChange(Sender: TObject);
    procedure NumberTCColorChanged(Sender: TObject);
    procedure NumberUChange(Sender: TObject);
    procedure OtherBChange(Sender: TObject);
    procedure OtherBGChange(Sender: TObject);
    procedure OtherFBChange(Sender: TObject);
    procedure OtherFCColorChanged(Sender: TObject);
    procedure OtherIChange(Sender: TObject);
    procedure OtherTCClick(Sender: TObject);
    procedure OtherUChange(Sender: TObject);
    procedure SavedColorButtonClick(Sender: TObject);
    procedure SavedColorPicklistChange(Sender: TObject);
    procedure ScrollPastEOLBoxChange(Sender: TObject);
    procedure SelectedColorButtonClick(Sender: TObject);
    procedure SelectedColorPicklistChange(Sender: TObject);
    procedure SelectedForeColorButtonClick(Sender: TObject);
    procedure SelectedForeColorPicklistChange(Sender: TObject);
    procedure SpaceBChange(Sender: TObject);
    procedure SpaceBGChange(Sender: TObject);
    procedure SpaceFBChange(Sender: TObject);
    procedure SpaceFCColorChanged(Sender: TObject);
    procedure SpaceIChange(Sender: TObject);
    procedure SpaceTCColorChanged(Sender: TObject);
    procedure SpaceUChange(Sender: TObject);
    procedure StringBChange(Sender: TObject);
    procedure StringBGChange(Sender: TObject);
    procedure StringFBChange(Sender: TObject);
    procedure StringFCColorChanged(Sender: TObject);
    procedure StringIChange(Sender: TObject);
    procedure StringTCColorChanged(Sender: TObject);
    procedure StringUChange(Sender: TObject);
    procedure SymbolBChange(Sender: TObject);
    procedure SymbolBGChange(Sender: TObject);
    procedure SymbolFBChange(Sender: TObject);
    procedure SymbolFCColorChanged(Sender: TObject);
    procedure SymbolIChange(Sender: TObject);
    procedure SymbolTCColorChanged(Sender: TObject);
    procedure SymbolUChange(Sender: TObject);
    procedure TabLenEditChange(Sender: TObject);
    procedure TextColorButtonClick(Sender: TObject);
    procedure TextColorPicklistChange(Sender: TObject);
    procedure TooltipColorButtonClick(Sender: TObject);
    procedure TooltipColorPicklistChange(Sender: TObject);
    procedure TooltipForeColorButtonClick(Sender: TObject);
    procedure TooltipForeColorPicklistChange(Sender: TObject);
    procedure VariableBChange(Sender: TObject);
    procedure VariableBGChange(Sender: TObject);
    procedure VariableFBChange(Sender: TObject);
    procedure VariableFCColorChanged(Sender: TObject);
    procedure VariableIChange(Sender: TObject);
    procedure VariableTCColorChanged(Sender: TObject);
    procedure VariableUChange(Sender: TObject);
  private
    FFuncList: TFuncList;
    { private declarations }
  public
    procedure Save(P: string);
    procedure Load(P: string);
    { public declarations }
  end;

var
  EditorConf: TEditorConf;

implementation

{$R *.lfm}

{ TEditorConf }


procedure TEditorConf.Save(P: string);

  procedure SaveGeneralData(FName: string);
  var
    conf: TEditorConfig;
    f: file of TEditorConfig;
  begin
    with conf do
    begin
      CERight := CESideBox.Checked;
      BGCol := BGColorPicklist.Selected;
      EditBGCol := EditorColorPicklist.Selected;
      GutterCol := GutterColorPicklist.Selected;
      GutterFore := GutterForeColorPicklist.Selected;
      GutterEdited := EditedColorPicklist.Color;
      GutterSaved := SavedColorPicklist.Selected;
      SelCol := SelectedColorPicklist.Selected;
      SelFCol := SelectedForeColorPicklist.Selected;
      TextColor := TextColorPicklist.Selected;
      PastEOL := ScrollPastEOLBox.Checked;
      CaretAV := CaretAlwaysVisibleBox.Checked;
      TabWidth := StrToInt(TabLenEdit.Text);
      TTipColor := TooltipColorPicklist.Selected;
      TTipFont := TooltipForeColorPicklist.Selected;
      EditorFont := EditorFrame1.CodeEditor.Font.FontData;
      FontName := EditorFrame1.Font.Name;
    end;
    AssignFile(f, FName);
    try
      Rewrite(f);
      Write(f, conf);
    finally
      CloseFile(f);
    end;
  end;

  procedure SaveHLFonts(FileName: string);
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
    if FileExists(FileName) then
      DeleteFile(FileName);
    fs := TFileStream.Create(FileName, fmCreate);
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

  procedure SaveSTDFunc(FileName: string);
  var
    sl, inf: TStringList;
    i, n: integer;
  begin
    sl := TStringList.Create;
    inf := TStringList.Create;
    try
      for i := 0 to FFuncList.Count - 1 do
      begin
        inf.Text := FFuncList[i].Info;
        for n := 0 to inf.Count - 1 do
          inf[n] := ';' + inf[n];
        sl.AddStrings(inf);
        sl.Add(FFuncList[i].Name);
      end;
      sl.SaveToFile(FileName);
    finally
      inf.Free;
      sl.Free;
    end;
  end;

  function GetHash(toHash: PChar; out Len: integer): byte;
  begin
    Result := 0;
    Len := 0;
    while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
    begin
      if (toHash^ in ['A'..'Z']) then
        Result := (Result + Ord(toHash^) - Ord('A') + Ord('a')) mod 256
      else
        Result := (Result + Ord(toHash^)) mod 256;
      Inc(ToHash);
      Inc(Len);
    end;
  end;

  procedure SaveHLKeywords(FileName: string);
  var
    i, n, a: integer;
    h: byte;
    tmp: PHashInfo;
    HL: array[0..255] of TList;
    fs: TFileStream;
  begin
    for i := 0 to 255 do
      HL[i] := TList.Create;
    try
      for i := 0 to KeyWordView.Items.Count - 1 do
      begin
        h := GetHash(PChar(LowerCase(KeyWordView.Items[i].Caption)), n);
        new(tmp);
        tmp^.Key := LowerCase(KeyWordView.Items[i].Caption);
        tmp^.Kind := TTokenType(IntPtr(KeyWordView.Items[i].Data));
        HL[h].Add(tmp);
      end;
      fs := TFileStream.Create(FileName, fmCreate);
      try
        for i := 0 to 255 do
        begin
          fs.Write(HL[i].Count, SizeOf(integer));
          for n := 0 to HL[i].Count - 1 do
          begin
            tmp := PHashInfo(HL[i][n]);
            fs.Write(tmp^.Kind, SizeOf(tmp^.Kind));
            a := Length(tmp^.Key);
            fs.Write(a, SizeOf(a));
            fs.Write(tmp^.Key[1], a);
          end;
        end;
      finally
        fs.Free;
      end;

    finally
      for i := 0 to 255 do
      begin
        for n := 0 to HL[i].Count - 1 do
          Dispose(PHashInfo(HL[i][n]));
        HL[i].Free;
      end;
    end;

  end;

  procedure SaveKeywords(Filename: string);
  var
    sl: TStringList;
    i: integer;
  begin
    sl := TStringList.Create;
    try
      for i := 0 to KeyWordView.Items.Count - 1 do
        sl.Add(KeyWordView.Items[i].Caption);
      sl.SaveToFile(Filename);
    finally
      sl.Free;
    end;
  end;

begin
  SaveGeneralData(IncludeTrailingPathDelimiter(p) + 'editor.cfg');
  SaveHLFonts(IncludeTrailingPathDelimiter(p) + 'HL' + PathDelim + 'colors.cnf');
  SaveSTDFunc(IncludeTrailingPathDelimiter(p) + 'Funcs.lst');
  SaveHLKeywords(IncludeTrailingPathDelimiter(p) + 'HL' + PathDelim + 'Keywords.lst');
  SaveKeywords(IncludeTrailingPathDelimiter(p) + 'Keywords.lst');
end;

procedure TEditorConf.Load(P: string);

  procedure LoadGeneralData(FName: string);
  var
    conf: TEditorConfig;
    f: file of TEditorConfig;
  begin
    AssignFile(f, FName);
    try
      Reset(f);
      Read(f, conf);
    finally
      CloseFile(f);
    end;
    with conf do
    begin
      CESideBox.Checked := CERight;
      BGColorPicklist.Selected := BGCol;
      EditorColorPicklist.Selected := EditBGCol;
      GutterColorPicklist.Selected := GutterCol;
      GutterForeColorPicklist.Selected := GutterFore;
      EditedColorPicklist.Color := GutterEdited;
      SavedColorPicklist.Selected := GutterSaved;
      SelectedColorPicklist.Selected := SelCol;
      SelectedForeColorPicklist.Selected := SelFCol;
      TextColorPicklist.Selected := TextColor;
      ScrollPastEOLBox.Checked := PastEOL;
      CaretAlwaysVisibleBox.Checked := CaretAV;
      TabLenEdit.Text := IntToStr(TabWidth);
      TooltipColorPicklist.Selected := TTipColor;
      TooltipForeColorPicklist.Selected := TTipFont;
      EditorFontButton.Caption := FontName;
    end;
  end;

  procedure LoadHLFonts(FName: string);
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
    fs := TFileStream.Create(FName, fmOpenRead);
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

  procedure LoadSTDFunc(FName: string);
  var
    sl, inf: TStringList;
    i: integer;
  begin
    FFuncList.Clear;
    FuncBox.Clear;
    sl := TStringList.Create;
    inf := TStringList.Create;
    try
      sl.LoadFromFile(FName);
      for i := 0 to sl.Count - 1 do
        if length(sl[i]) > 0 then
          if sl[i][1] = ';' then
            inf.Add(Copy(sl[i], 2, Length(sl[i]) - 1))
          else
          begin
            FFuncList.Add(FuncInfo(sl[i], 0, inf.Text));
            FuncBox.Items.Add(sl[i]);
            inf.Clear;
          end;
    finally
      sl.Free;
      inf.Free;
    end;
  end;

  procedure LoadHLKeywords(FName: string);

    function FindItem(n: string): TListItem;
    var
      i: integer;
    begin
      Result := nil;
      for i := 0 to KeyWordView.Items.Count - 1 do
        if LowerCase(KeyWordView.Items[i].Caption) = LowerCase(n) then
        begin
          Result := KeyWordView.Items[i];
          break;
        end;
    end;

  var
    fs: TFileStream;
    i, x, a, ln: integer;
    n: string;
    itm: TListItem;
    k: TTokenType;
  begin
    fs := TFileStream.Create(FName, fmOpenRead);
    try
      for x := 0 to 255 do
      begin
        fs.Read(a, SizeOf(integer));
        for i := 0 to a - 1 do
        begin
          fs.Read(k, SizeOf(k));
          fs.Read(ln, SizeOf(integer));
          SetLength(n, ln);
          fs.Read(n[1], ln);
          itm := FindItem(n);
          if not Assigned(itm) then
          begin
            itm := KeyWordView.Items.Add;
            itm.Caption := n;
          end;
          if itm.SubItems.Count = 0 then
          begin
            itm.SubItems.Add(KeyTypeBox.Items[Ord(k)]);
            itm.Data := Pointer(Ord(k));
          end;
        end;
      end;
    finally
      fs.Free;
    end;
  end;

  procedure LoadKeywords(FName: string);
  var
    sl: TStringList;
    i: integer;
  begin
    KeyWordView.Clear;
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FName);
      for i := 0 to sl.Count - 1 do
      begin
        KeyWordView.Items.Add.Caption := sl[i];
      end;
    finally
      sl.Free;
    end;
  end;

begin
  LoadGeneralData(IncludeTrailingPathDelimiter(p) + 'editor.cfg');
  LoadHLFonts(IncludeTrailingPathDelimiter(p) + 'HL' + PathDelim + 'colors.cnf');
  LoadSTDFunc(IncludeTrailingPathDelimiter(p) + 'Funcs.lst');
  LoadKeywords(IncludeTrailingPathDelimiter(p) + 'Keywords.lst');
  LoadHLKeywords(IncludeTrailingPathDelimiter(p) + 'HL' + PathDelim + 'Keywords.lst');
  EditorFrame1.ReLoadConf;
end;

procedure TEditorConf.CESideBoxChange(Sender: TObject);
begin
  CESideBox.Caption := IfThen(CESideBox.Checked, 'Rechts', 'Links');
  if CESideBox.Checked then
  begin
    EditorFrame1.CodeExplorerPanel.Align := alRight;
    EditorFrame1.CESplitter.Align := alRight;
  end
  else
  begin
    EditorFrame1.CodeExplorerPanel.Align := alLeft;
    EditorFrame1.CESplitter.Align := alLeft;
  end;
end;

procedure TEditorConf.CommentBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.CommentAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.CommentBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.CommentAttribute do
    if CommentBG.Checked then
      Background := CommentBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.CommentFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.CommentAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.CommentFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.CommentAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.CommentIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.CommentAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.CommentTCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.CommentAttribute.Foreground := CommentTC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.CommentUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.CommentAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.DeleteFuncButtonClick(Sender: TObject);
begin
  if FuncBox.ItemIndex >= 0 then
  begin
    FFuncList.Delete(FuncBox.ItemIndex);
    FuncBox.Items.Delete(FuncBox.ItemIndex);
  end;
end;

procedure TEditorConf.DeleteKeywordButtonClick(Sender: TObject);
begin
  KeyWordView.Selected.Free;
  DeleteKeywordButton.Enabled := False;
end;

procedure TEditorConf.DocBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.DocumentaryAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.DocBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.DocumentaryAttribute do
    if DocBG.Checked then
      Background := DocBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.DocFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.DocumentaryAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.DocFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.DocumentaryAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.DocIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.DocumentaryAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.DocTCClick(Sender: TObject);
begin
  EditorFrame1.Highlighter.DocumentaryAttribute.Foreground := DocTC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.DocUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.DocumentaryAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.EditedColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := EditedColorPicklist.Selected;
  if ColorDialog1.Execute then
    EditedColorPicklist.Selected := ColorDialog1.Color;
  if EditedColorPicklist.Text = '' then
    EditedColorPicklistChange(EditedColorPicklist);
end;

procedure TEditorConf.EditedColorPicklistChange(Sender: TObject);
var
  i: integer;
begin
  with EditorFrame1.CodeEditor.Gutter do
    for i := 0 to Parts.Count - 1 do
      if Parts[i] is TSynGutterChanges then
        (Parts[i] as TSynGutterChanges).ModifiedColor :=
          EditedColorPicklist.Selected;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.EditedColorPicklistGetColors(Sender: TCustomColorBox;
  Items: TStrings);
begin
  Items.Add((Sender as TColorBox).Text);
end;

procedure TEditorConf.EditorColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := EditorColorPicklist.Selected;
  if ColorDialog1.Execute then
    EditorColorPicklist.Selected := ColorDialog1.Color;
  if EditorColorPicklist.Text = '' then
    EditorColorPicklistChange(EditorColorPicklist);
end;

procedure TEditorConf.EditorColorPicklistChange(Sender: TObject);
begin
  EditorFrame1.CodeEditor.Color := EditorColorPicklist.Selected;
  EditorFrame1.CodeExplorer.BackgroundColor := EditorColorPicklist.Selected;
  EditorFrame1.SearchEdit.Color := EditorColorPicklist.Selected;
  EditorFrame1.ReplaceEdit.Color := EditorColorPicklist.Selected;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.EditorFontButtonClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(EditorFrame1.CodeEditor.Font);
  if FontDialog1.Execute then
  begin
    EditorFontButton.Caption := FontDialog1.Font.Name;
    EditorFrame1.CodeEditor.Font.Assign(FontDialog1.Font);
    EditorFrame1.CodeEditor.Invalidate;
  end;
end;

procedure TEditorConf.FormCreate(Sender: TObject);
begin
  FFuncList := TFuncList.Create;
end;

procedure TEditorConf.FormDestroy(Sender: TObject);
begin
  FFuncList.Free;
end;

procedure TEditorConf.FormShow(Sender: TObject);
begin
  EditorFrame1.CodeEditor.TextBetweenPoints[Point(1, 1), Point(2, 1)] := ';';
  EditorFrame1.CodeEditor.MarkTextAsSaved;
  EditorFrame1.CodeEditor.TextBetweenPoints[Point(1, 3), Point(2, 3)] := ';';
end;

procedure TEditorConf.FuncBoxClick(Sender: TObject);
begin
  DeleteFuncButton.Enabled := FuncBox.ItemIndex >= 0;
  if FuncBox.ItemIndex >= 0 then
  begin
    FuncNameEdit.Text := FFuncList[FuncBox.ItemIndex].Name;
    FuncInfoMemo.Text := FFuncList[FuncBox.ItemIndex].Info;
  end;
end;

procedure TEditorConf.FuncNameEditChange(Sender: TObject);
begin
  AddFuncButton.Enabled := Length(FuncNameEdit.Text) > 0;
end;

procedure TEditorConf.FunctionBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.FunctionAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.FunctionBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.FunctionAttribute do
    if FunctionBG.Checked then
      Background := FunctionBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.FunctionFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.FunctionAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.FunctionFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.FunctionAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.FunctionIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.FunctionAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.FunctionTCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.FunctionAttribute.Foreground := FunctionFC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.FunctionUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.FunctionAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.GutterColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := GutterColorPicklist.Selected;
  if ColorDialog1.Execute then
    GutterColorPicklist.Selected := ColorDialog1.Color;
  if GutterColorPicklist.Text = '' then
    GutterColorPicklistChange(GutterColorPicklist);
end;

procedure TEditorConf.GutterColorPicklistChange(Sender: TObject);
var
  i: integer;
begin
  with EditorFrame1.CodeEditor.Gutter do
  begin
    Color := GutterColorPicklist.Selected;
    for i := 0 to Parts.Count - 1 do
      if Parts[i] is TSynGutterPartBase then
        (Parts[i] as TSynGutterPartBase).MarkupInfo.Background :=
          GutterColorPicklist.Selected;
  end;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.GutterForeColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := GutterForeColorPicklist.Selected;
  if ColorDialog1.Execute then
    GutterForeColorPicklist.Selected := ColorDialog1.Color;
  if GutterForeColorPicklist.Text = '' then
    GutterForeColorPicklistChange(GutterForeColorPicklist);
end;

procedure TEditorConf.GutterForeColorPicklistChange(Sender: TObject);
var
  i: integer;
begin
  with EditorFrame1.CodeEditor.Gutter do
    for i := 0 to Parts.Count - 1 do
      if Parts[i] is TSynGutterPartBase then
        (Parts[i] as TSynGutterPartBase).MarkupInfo.Foreground :=
          GutterForeColorPicklist.Selected;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.IdentifierBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.IdentifierAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.IdentifierBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.IdentifierAttribute do
    if IdentifierBG.Checked then
      Background := IdentifierBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.IdentifierFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.IdentifierAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.IdentifierFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.IdentifierAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.IdentifierIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.IdentifierAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.IdentifierTCClick(Sender: TObject);
begin
  EditorFrame1.Highlighter.IdentifierAttribute.Foreground := IdentifierTC.ButtonColor;
end;

procedure TEditorConf.IdentifierTCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.IdentifierAttribute.Foreground := IdentifierTC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.IdentifierUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.IdentifierAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.KeyEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 13 then
    AddKeywordButtonClick(AddKeywordButton);
end;

procedure TEditorConf.KeyWordViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
begin
  DeleteKeywordButton.Enabled := Assigned(Item);
end;

procedure TEditorConf.NumberBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.NumberAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.NumberBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.NumberAttribute do
    if CommentBG.Checked then
      Background := CommentBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.NumberFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.NumberAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.NumberFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.NumberAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.NumberIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.NumberAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.NumberTCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.NumberAttribute.Foreground := NumberTC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.NumberUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.NumberAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.OtherBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.TextAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.OtherBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.TextAttribute do
    if OtherBG.Checked then
      Background := OtherBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.OtherFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.TextAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.OtherFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.TextAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.OtherIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.TextAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.OtherTCClick(Sender: TObject);
begin
  EditorFrame1.Highlighter.TextAttribute.Foreground := OtherTC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.OtherUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.TextAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SavedColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := BGColorPicklist.Selected;
  if ColorDialog1.Execute then
    BGColorPicklist.Selected := ColorDialog1.Color;

  if SavedColorPicklist.Text = '' then
    SavedColorPicklistChange(SavedColorPicklist);
end;

procedure TEditorConf.SavedColorPicklistChange(Sender: TObject);
var
  i: integer;
begin
  with EditorFrame1.CodeEditor.Gutter do
    for i := 0 to Parts.Count - 1 do
      if Parts[i] is TSynGutterChanges then
        (Parts[i] as TSynGutterChanges).SavedColor :=
          SavedColorPicklist.Selected;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.ScrollPastEOLBoxChange(Sender: TObject);
begin
  if ScrollPastEOLBox.Checked then
    EditorFrame1.CodeEditor.Options :=
      [eoScrollPastEol] + EditorFrame1.CodeEditor.Options
  else
    EditorFrame1.CodeEditor.Options :=
      [eoScrollPastEol] - EditorFrame1.CodeEditor.Options;
end;

procedure TEditorConf.SelectedColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := SelectedColorPicklist.Selected;
  if ColorDialog1.Execute then
    SelectedColorPicklist.Selected := ColorDialog1.Color;

  if SelectedColorPicklist.Text = '' then
    SelectedColorPicklistChange(SelectedColorPicklist);
end;

procedure TEditorConf.SelectedColorPicklistChange(Sender: TObject);
begin
  EditorFrame1.CodeExplorer.SelectionColor := SelectedColorPicklist.Selected;
  EditorFrame1.CodeEditor.SelectedColor.Background := SelectedColorPicklist.Selected;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.SelectedForeColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := SelectedForeColorPicklist.Selected;
  if ColorDialog1.Execute then
    SelectedForeColorPicklist.Selected := ColorDialog1.Color;

  if SelectedForeColorPicklist.Text = '' then
    SelectedForeColorPicklistChange(SelectedForeColorPicklist);
end;

procedure TEditorConf.SelectedForeColorPicklistChange(Sender: TObject);
begin
  EditorFrame1.CodeEditor.SelectedColor.Foreground := SelectedForeColorPicklist.Selected;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.SpaceBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SpaceAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SpaceBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SpaceAttribute do
    if SpaceBG.Checked then
      Background := SpaceBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SpaceFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SpaceAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SpaceFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.SpaceAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SpaceIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SpaceAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SpaceTCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.SpaceAttribute.Foreground := SpaceTC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SpaceUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SpaceAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.StringBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.StringAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.StringBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.StringAttribute do
    if StringBG.Checked then
      Background := StringBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.StringFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.StringAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.StringFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.StringAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.StringIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.StringAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.StringTCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.StringAttribute.Foreground := StringTC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.StringUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.StringAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SymbolBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SymbolAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SymbolBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SymbolAttribute do
    if SymbolBG.Checked then
      Background := SymbolBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SymbolFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SymbolAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SymbolFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.SymbolAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SymbolIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SymbolAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SymbolTCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.SymbolAttribute.Foreground := SymbolTC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.SymbolUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.SymbolAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.TabLenEditChange(Sender: TObject);
begin
  if TabLenEdit.Text <> '' then
    EditorFrame1.CodeEditor.TabWidth := StrToInt(TabLenEdit.Text);
end;

procedure TEditorConf.TextColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := TextColorPicklist.Selected;
  if ColorDialog1.Execute then
    TextColorPicklist.Selected := ColorDialog1.Color;

  if TextColorPicklist.Text = '' then
    TextColorPicklistChange(TextColorPicklist);
end;

procedure TEditorConf.TextColorPicklistChange(Sender: TObject);
begin
  EditorFrame1.Font.Color := TextColorPicklist.Selected;
  EditorFrame1.CodeExplorer.Color := TextColorPicklist.Selected;
  EditorFrame1.CodeExplorer.ExpandSignColor :=
    GetHighLightColor(TextColorPicklist.Selected);
  EditorFrame1.CodeExplorer.TreeLineColor :=
    GetHighLightColor(TextColorPicklist.Selected);
  EditorFrame1.CodeExplorer.SeparatorColor :=
    GetHighLightColor(TextColorPicklist.Selected);
  EditorFrame1.CodeExplorer.Invalidate;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.TooltipColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := TooltipColorPicklist.Selected;
  if ColorDialog1.Execute then
    TooltipColorPicklist.Selected := ColorDialog1.Color;

  if TooltipColorPicklist.Text = '' then
    TooltipColorPicklistChange(TooltipColorPicklist);
end;

procedure TEditorConf.TooltipColorPicklistChange(Sender: TObject);
begin
  EditorFrame1.ToolTip.Color := TooltipColorPicklist.Selected;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.TooltipForeColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := TooltipForeColorPicklist.Selected;
  if ColorDialog1.Execute then
    TooltipForeColorPicklist.Selected := ColorDialog1.Color;

  if TooltipForeColorPicklist.Text = '' then
    TooltipForeColorPicklistChange(TooltipForeColorPicklist);
end;

procedure TEditorConf.TooltipForeColorPicklistChange(Sender: TObject);
begin
  EditorFrame1.ToolTip.Font.Color := TooltipForeColorPicklist.Selected;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.VariableBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.VariableAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.VariableBGChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.VariableAttribute do
    if VariableBG.Checked then
      Background := VariableBC.ButtonColor
    else
      Background := clNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.VariableFBChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.VariableAttribute do
    if (Sender as TToggleBox).Checked then
      FrameEdges := sfeAround
    else
      FrameEdges := sfeNone;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.VariableFCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.VariableAttribute.FrameColor :=
    (Sender as TColorButton).ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.VariableIChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.VariableAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.VariableTCColorChanged(Sender: TObject);
begin
  EditorFrame1.Highlighter.VariableAttribute.Foreground := VariableTC.ButtonColor;
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.VariableUChange(Sender: TObject);
begin
  with EditorFrame1.Highlighter.VariableAttribute do
    if (Sender as TToggleBox).Checked then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  EditorFrame1.CodeEditor.Invalidate;
end;

procedure TEditorConf.BGColorBtnClick(Sender: TObject);
begin
  ColorDialog1.Color := BGColorPicklist.Selected;
  if ColorDialog1.Execute then
    BGColorPicklist.Selected := ColorDialog1.Color;

  if BGColorPicklist.Text = '' then
    BGColorPicklistChange(BGColorPicklist);
end;

procedure TEditorConf.AddKeywordButtonClick(Sender: TObject);
begin
  if (KeyEdit.Text <> '') and (KeyWordView.FindCaption(0, KeyEdit.Text,
    False, True, False) = nil) then
    with KeyWordView.Items.Add do
    begin
      Caption := KeyEdit.Text;
      SubItems.Add(KeyTypeBox.Text);
      Data := Pointer(KeyTypeBox.ItemIndex);
    end;
end;

procedure TEditorConf.AddFuncButtonClick(Sender: TObject);
begin
  if (FuncNameEdit.Text <> '') and not StringsContain(FuncBox.Items,
    FuncNameEdit.Text) then
  begin
    FFuncList.Add(FuncInfo(FuncNameEdit.Text, 0, FuncInfoMemo.Text));
    FuncBox.Items.Add(FuncNameEdit.Text);
  end;
end;

procedure TEditorConf.BGColorPicklistChange(Sender: TObject);
begin
  EditorFrame1.Color := BGColorPicklist.Selected;
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TEditorConf.CaretAlwaysVisibleBoxChange(Sender: TObject);
begin
  if CaretAlwaysVisibleBox.Checked then
    EditorFrame1.CodeEditor.Options2 :=
      [eoAlwaysVisibleCaret] + EditorFrame1.CodeEditor.Options2
  else
    EditorFrame1.CodeEditor.Options2 :=
      [eoAlwaysVisibleCaret] - EditorFrame1.CodeEditor.Options2;
end;

end.
