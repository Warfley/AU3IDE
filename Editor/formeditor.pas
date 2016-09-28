unit FormEditor;

{ TODO : Impelement Undo, Redo for Creation/Deletion of Components }
{ TODO : Persitent Undo }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, RTTIGrids, Forms, Controls,
  Graphics, ExtCtrls, StdCtrls, ValEdit, ComCtrls, Grids, contnrs, au3Types,
  Dialogs, FormEditComponents, LCLIntf, Math, GraphUtil, PropEdits, ObjectInspector, TLStrings, LCLTranslator;

type

  { TFormEditFrame }

  TFormEditFrame = class(TFrame)
    FormCaptionLabel: TLabel;
    ImageList1: TImageList;
    EventEditor: TValueListEditor;
    PositionPickerPanel: TPanel;
    PositionPicker: TPaintBox;
    PropEditor: TTIPropertyGrid;
    OISplitter: TSplitter;
    ToolSelect: TListView;
    ToolboxHeaderPanel: TPanel;
    ToolBoxPanel: TPanel;
    PropertyPages: TPageControl;
    PropertyPanel: TPanel;
    ControlProps: TTabSheet;
    ControlEvents: TTabSheet;
    EditorScrollBox: TScrollBox;
    TreeFilterEdit1: TTreeFilterEdit;
    FormControlView: TTreeView;
    procedure EditorScrollBoxPaint(Sender: TObject);
    procedure EventEditorEditingDone(Sender: TObject);
    procedure EventEditorGetPickList(Sender: TObject; const KeyName: string;
      Values: TStrings);
    procedure EventEditorPickListSelect(Sender: TObject);
    procedure FormControlViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormControlViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure FormControlViewKeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormPanelDblClick(Sender: TObject);
    procedure FormPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormPanelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure FormPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormPanelPaint(Sender: TObject);
    procedure OISplitterMoved(Sender: TObject);
    procedure PositionPickerMouseEnter(Sender: TObject);
    procedure PositionPickerMouseLeave(Sender: TObject);
    procedure PositionPickerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure PositionPickerPaint(Sender: TObject);
    procedure PropEditorEditorFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure PropertyPanelResize(Sender: TObject);
    procedure ToolboxHeaderPanelClick(Sender: TObject);
    procedure ToolboxHeaderPanelMouseEnter(Sender: TObject);
    procedure ToolboxHeaderPanelMouseLeave(Sender: TObject);
    procedure PickListClick(Sender: TObject);
  private
    FFormular: Tau3Form;
    MovingControl: TControl;
    FChangeProps: boolean;
    FFileName: string;
    FConf: TFormEditorConfig;
    UndoStack, RedoStack: TPropChangeStack;
    Moved: boolean;
    FLastClickTime: cardinal;
    FLastClickRow: integer;
    sizing: boolean;
    FDrawLines: boolean;
    FMousePoint: TPoint;
    FPanelMousePoint: TPoint;
    FSelPoint: TPoint;
    FCopyLst: TObjectList;
    FOldLeft, FOldTop: integer;
    FStartTop, FStartLeft: integer;
    FOnChange: TNotifyEvent;
    FOpenEditor: TOpenEditorEvent;
    FEnterFunc: TOpenFunctionEvent;
    FOnVarChanged: TNotifyEvent;
    FFuncList: TStringList;
    FMaxUndoSize: integer;
    FBorderHeight, FBorderWidth: Integer;
    { private declarations }
    function CopyControl(c: TObject): TObject;
    procedure DeleteItem(n: TTreeNode);
    function FindControl(s: string): integer;
    function CreateButton(P: TWinControl): Tau3Button;
    function CreateCheckBox(P: TWinControl): Tau3CheckBox;
    function CreateLabel(P: TWinControl): Tau3Label;
    function CreateEdit(P: TWinControl): Tau3Edit;
    procedure LoadControlData(c: TComponent);
    procedure PropChanged(Sender: TObject; PropName, PropVal, OldVal: string);
    procedure SetBorderHeight(AValue: Integer);
    procedure SetBorderWidth(AValue: Integer);
    procedure SetMaxUndoSize(AValue: integer);
    procedure UpdateFormCaption(Sender: TObject);
    procedure PushOnUndo(c: TChangeData);
  public
    procedure ReLoadConf;
    procedure DoUndo;
    procedure DoRedo;
    constructor Create(TheOwner: TComponent); override;
    procedure AddToVarlist(l: TVarList);
    destructor Destroy; override;
    procedure Save(p: string = '');
    procedure Load(p: string = '');
    procedure SetMainForm(b: boolean; Silent: boolean = False);
    { public declarations }
    property FuncList: TStringList read FFuncList;
    property FileName: string read FFileName write FFileName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OpenEditor: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    property EnterFunc: TOpenFunctionEvent read FEnterFunc write FEnterFunc;
    property OnVarChanged: TNotifyEvent read FOnVarChanged write FOnVarChanged;
    property MaxUndoSize: integer read FMaxUndoSize write SetMaxUndoSize;
    property BorderHeight: Integer read FBorderHeight write SetBorderHeight;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
  end;

function ChangeDataToString(c: TChangeData): string; inline;

implementation

{$R *.lfm}

function ChangeDataToString(c: TChangeData): string; inline;
begin
  Result := Format('[%s]%s: %s->%s', [(c.comp as TControl).Name,
    c.Prop, c.OldVal, c.NewVal]);
end;

procedure TFormEditFrame.UpdateFormCaption(Sender: TObject);
begin
  FormCaptionLabel.Caption := FFormular.Caption;
end;

procedure TFormEditFrame.PushOnUndo(c: TChangeData);
var
  tmp: TPropChangeStack;
  i: integer;
begin
  if UndoStack.Size() < MaxUndoSize then
  begin
    UndoStack.Push(c);
    Exit;
  end;
  tmp := TPropChangeStack.Create;
  try
    i := 1;
    tmp.Push(c);
    while not UndoStack.IsEmpty() do
    begin
      if i < MaxUndoSize then
        tmp.Push(UndoStack.Top());
      Inc(i);
      UndoStack.Pop();
    end;
    while not tmp.IsEmpty() do
    begin
      UndoStack.Push(tmp.Top());
      tmp.Pop();
    end;
  finally
    tmp.Free;
  end;
end;

procedure TFormEditFrame.PropChanged(Sender: TObject; PropName, PropVal, OldVal: string);
var
  i: integer;
  undoItem: TChangeData;
begin
  if (not FChangeProps) and (FormControlView.SelectionCount > 0) then
  begin
    FChangeProps := True;
    try
      undoItem.comp := Sender;
      undoItem.Prop := PropName;
      undoItem.NewVal := PropVal;
      undoItem.OldVal := OldVal;
      if Length(PropName) > 0 then
        PushOnUndo(undoItem);

      PropName := LowerCase(PropName);
      if PropName = 'name' then
      begin
        for i := 0 to FormControlView.Items.Count - 1 do
          if TObject(FormControlView.Items[i].Data) = Sender then
          begin
            FormControlView.Items[i].Text := PropVal;
            break;
          end;
      end
      else
      begin
        for i := 0 to FormControlView.Items.Count - 1 do
          if FormControlView.Items[i].Selected and
            (TObject(FormControlView.Items[i].Data) as
            Iau3Component).CheckProperty(PropName) and
            (Sender <> TObject(FormControlView.Items[i].Data)) then
          begin
            undoItem.OldVal :=
              (TObject(FormControlView.Items[i].Data) as
              Iau3Component).GetProp(PropName);
            (TObject(FormControlView.Items[i].Data) as Iau3Component).SetProp(
              PropName, PropVal);
            undoItem.comp := TObject(FormControlView.Items[i].Data);
            UndoStack.Push(undoItem);
          end;
        (Sender as TControl).Parent.Invalidate;
      end;
    finally
      FChangeProps := False;
    end;
    Parent.Caption := '*' + ExtractFileName(FFileName);
  end;
end;

procedure TFormEditFrame.SetBorderHeight(AValue: Integer);
begin
  if FBorderHeight=AValue then Exit;
  FBorderHeight:=AValue;
  FFormular.BorderHeight:=AValue;
end;

procedure TFormEditFrame.SetBorderWidth(AValue: Integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  FFormular.BorderWidth:=AValue;
end;

procedure TFormEditFrame.SetMaxUndoSize(AValue: integer);
var
  tmp: TPropChangeStack;
  i: integer;
begin
  if FMaxUndoSize = AValue then
    Exit;
  tmp := TPropChangeStack.Create;
  try
    i := 0;
    while not UndoStack.IsEmpty do
    begin
      if (i < AValue) then
        tmp.Push(UndoStack.Top());
      Inc(i);
      UndoStack.Pop();
    end;
    while not tmp.IsEmpty do
    begin
      UndoStack.Push(tmp.Top());
      tmp.Pop();
    end;
  finally
    tmp.Free;
  end;
  FMaxUndoSize := AValue;
end;

procedure TFormEditFrame.ReLoadConf;
var
  f: file of TFormEditorConfig;
begin
  AssignFile(f, IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'foms.cfg');
  try
    Reset(f);
    Read(f, FConf);
  finally
    CloseFile(f);
  end;
  with FConf do
  begin
    if OIRight then
    begin
      PropertyPanel.Align := alRight;
      OISplitter.Align := alRight;
      PositionPickerPanel.Left :=
        ClientWidth - PropertyPanel.Width - 8 - PositionPickerPanel.Width;
      ToolBoxPanel.Left := 8;
    end
    else
    begin
      PropertyPanel.Align := alLeft;
      OISplitter.Align := alLeft;
      PositionPickerPanel.Left := ClientWidth - 8 - PositionPickerPanel.Width;
      ToolBoxPanel.Left := PropertyPanel.Width + 8;
    end;
    Color := BGCol;
    FormControlView.BackgroundColor := BGCol;
    EditorScrollBox.Color := BGCol;
    PropertyPages.Color := BGCol;
    PropertyPanel.Color := BGCol;
    PropEditor.Color := BGCol;
    EventEditor.Color := BGCol;
    TreeFilterEdit1.Color := BGCol;
    TreeFilterEdit1.Font.Color := ForeCol;
    Font.Color := ForeCol;
    FFormular.DoubleBuffered := DoubleBuffer;
    ToolboxHeaderPanel.Color := TBCol;
    ToolboxHeaderPanel.Font.Color := ForeCol;
    FormControlView.Color := ForeCol;
    FormControlView.ExpandSignColor := GetHighLightColor(ForeCol);
    FormControlView.TreeLineColor := GetHighLightColor(ForeCol);
    FormControlView.SeparatorColor := GetHighLightColor(ForeCol);
    Invalidate;
  end;
end;

procedure TFormEditFrame.DoUndo;
var
  undoitem: TChangeData;
begin
  if UndoStack.Size > 0 then
  begin
    undoitem := UndoStack.Top();
    UndoStack.Pop();
    RedoStack.Push(undoitem);
    FChangeProps := True;
    try
      with undoitem do
        (comp as Iau3Component).SetProp(Prop, OldVal);
    finally
      FChangeProps := False;
    end;
    FFormular.Invalidate;
  end;
end;

procedure TFormEditFrame.DoRedo;
var
  redoitem: TChangeData;
begin
  if RedoStack.Size > 0 then
  begin
    redoitem := RedoStack.Top();
    RedoStack.Pop();
    UndoStack.Push(redoitem);
    FChangeProps := True;
    try
      with redoitem do
        (comp as Iau3Component).SetProp(Prop, NewVal);
    finally
      FChangeProps := False;
    end;
    FFormular.Invalidate;
  end;
end;

procedure TFormEditFrame.LoadControlData(c: TComponent);
begin
  (c as Iau3Component).FillEvents(EventEditor);
  PropEditor.TIObject := c;
  PropEditor.Refresh;
end;

function TFormEditFrame.FindControl(s: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FormControlView.Items.Count - 1 do
    if LowerCase((TObject(FormControlView.Items[i].Data) as TControl).Name) =
      LowerCase(s) then
    begin
      Result := i;
      Break;
    end;
end;

function TFormEditFrame.CreateButton(P: TWinControl): Tau3Button;
var
  i: integer;
begin
  Result := Tau3Button.Create(FFormular);
  Result.Parent := P;
  i := 1;
  while FindControl('Button' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'Button' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.OnDblClick := @FormPanelDblClick;
  Result.Top := FPanelMousePoint.Y;
  Result.OnMouseDown := @FormPanelMouseDown;
  Result.OnMouseUp := @FormPanelMouseUp;
  Result.OnKeyUp := @FormControlViewKeyUp;
  Result.OnMouseMove := @FormPanelMouseMove;
  Result.OnChangeProp := @PropChanged;
  Result.Tag := 0;
  for i := 0 to FormControlView.Items.Count - 1 do
    if FormControlView.Items[i].Data = Pointer(P) then
      with FormControlView.Items.AddChild(FormControlView.Items[i], Result.Name) do
      begin
        TreeView.Items[i].Expand(False);
        Data := Result;
        ImageIndex := 1;
        SelectedIndex := 1;
        FormControlView.ClearSelection;
        Selected := True;
        Break;
      end;
end;

function TFormEditFrame.CreateCheckBox(P: TWinControl): Tau3CheckBox;
var
  i: integer;
begin
  Result := Tau3Checkbox.Create(FFormular);
  Result.Parent := P;
  i := 1;
  while FindControl('CheckBox' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'CheckBox' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.Top := FPanelMousePoint.Y;
  Result.OnDblClick := @FormPanelDblClick;
  Result.OnMouseDown := @FormPanelMouseDown;
  Result.OnMouseUp := @FormPanelMouseUp;
  Result.OnKeyUp := @FormControlViewKeyUp;
  Result.OnMouseMove := @FormPanelMouseMove;
  Result.OnChangeProp := @PropChanged;
  Result.Tag := 0;
  for i := 0 to FormControlView.Items.Count - 1 do
    if FormControlView.Items[i].Data = Pointer(P) then
      with FormControlView.Items.AddChild(FormControlView.Items[i], Result.Name) do
      begin
        TreeView.Items[i].Expand(False);
        Data := Result;
        ImageIndex := 2;
        SelectedIndex := 2;
        FormControlView.ClearSelection;
        Selected := True;
        Break;
      end;
end;

procedure TFormEditFrame.PickListClick(Sender: TObject);
var
  c: cardinal;
begin
  c := GetTickCount;
  if (FLastClickRow = EventEditor.Row) and (c - FLastClickTime < 700) and
    (EventEditor.ScreenToClient(Mouse.CursorPos).x < EventEditor.Width - 20) then
  begin
    if EventEditor.Rows[EventEditor.Row][1] = '' then
      EventEditor.Rows[EventEditor.Row][1] := Format('(%s...)', [SNew]);
    EventEditorPickListSelect(nil);
  end;
  FLastClickTime := c;
  FLastClickRow := EventEditor.Row;
end;

function TFormEditFrame.CopyControl(c: TObject): TObject;
var n: String;
begin
  if c is Tau3Button then
    Result:=CreateButton((c as TControl).Parent)
  else if c is Tau3Label then
    Result:=CreateLabel((c as TControl).Parent)
  else if c is Tau3Edit then
    Result:=CreateEdit((c as TControl).Parent)
  else if c is Tau3Checkbox then
    Result:=CreateCheckBox((c as TControl).Parent);
  n:=(c as TControl).Name;
  while FindControl(n)>0 do
    n:=FindNewName(n);
  (Result as TControl).Name:=n;
  (c as Iau3Component).CopyTo(Result as TControl);
end;

function TFormEditFrame.CreateLabel(P: TWinControl): Tau3Label;
var
  i: integer;
begin
  Result := Tau3Label.Create(FFormular);
  Result.Parent := P;
  i := 1;
  while FindControl('Label' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'Label' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.Top := FPanelMousePoint.Y;
  Result.OnDblClick := @FormPanelDblClick;
  Result.OnKeyUp := @FormControlViewKeyUp;
  Result.OnMouseDown := @FormPanelMouseDown;
  Result.OnMouseUp := @FormPanelMouseUp;
  Result.Caption := Result.Name;
  Result.Width := Result.Canvas.TextWidth(Result.Caption);
  Result.Height := Result.Canvas.TextHeight(Result.Caption);
  Result.OnMouseMove := @FormPanelMouseMove;
  Result.OnChangeProp := @PropChanged;
  Result.Tag := 0;
  for i := 0 to FormControlView.Items.Count - 1 do
    if FormControlView.Items[i].Data = Pointer(P) then
      with FormControlView.Items.AddChild(FormControlView.Items[i], Result.Name) do
      begin
        TreeView.Items[i].Expand(False);
        Data := Result;
        ImageIndex := 4;
        SelectedIndex := 4;
        FormControlView.ClearSelection;
        Selected := True;
        Break;
      end;
end;

function TFormEditFrame.CreateEdit(P: TWinControl): Tau3Edit;
var
  i: integer;
begin
  Result := Tau3Edit.Create(FFormular);
  Result.Parent := P;
  i := 1;
  while FindControl('Edit' + IntToStr(i)) >= 0 do
    Inc(i);
  Result.Name := 'Edit' + IntToStr(i);
  Result.Left := FPanelMousePoint.x;
  Result.Top := FPanelMousePoint.Y;
  Result.ReadOnly := True;
  Result.OnMouseDown := @FormPanelMouseDown;
  Result.OnMouseUp := @FormPanelMouseUp;
  Result.OnDblClick := @FormPanelDblClick;
  Result.OnMouseMove := @FormPanelMouseMove;
  Result.OnKeyUp := @FormControlViewKeyUp;
  Result.OnChangeProp := @PropChanged;
  Result.Tag := 0;
  for i := 0 to FormControlView.Items.Count - 1 do
    if FormControlView.Items[i].Data = Pointer(P) then
      with FormControlView.Items.AddChild(FormControlView.Items[i], Result.Name) do
      begin
        TreeView.Items[i].Expand(False);
        Data := Result;
        ImageIndex := 3;
        SelectedIndex := 3;
        FormControlView.ClearSelection;
        Selected := True;
        Break;
      end;
end;

constructor TFormEditFrame.Create(TheOwner: TComponent);
begin
  inherited;
  FFuncList := TStringList.Create;
  FDrawLines := False;
  EventEditor.EditorByStyle(cbsPickList).OnClick := @PickListClick;
  EventEditor.EditorByStyle(cbsPickList).OnEnter := @PickListClick;
  FMousePoint := Point(-1, -1);
  FChangeProps := False;
  FFormular := Tau3Form.Create(EditorScrollBox);
  FFormular.Parent := EditorScrollBox;
  FFormular.Height := 312;
  FFormular.Width := 386;
  FFormular.SetFormPos(16, 32);
  FFormular.Visible := True;
  FFormular.Color := clBtnFace;
  FFormular.OnPaint := @FormPanelPaint;
  FFormular.OnDblClick := @FormPanelDblClick;
  FFormular.OnMouseDown := @FormPanelMouseDown;
  FFormular.OnMouseMove := @FormPanelMouseMove;
  FFormular.OnMouseUp := @FormPanelMouseUp;
  FFormular.OnChangeProp := @PropChanged;
  FFormular.OnChangeCaption := @UpdateFormCaption;
  FormControlView.Items.Add(nil, 'Form1').Data := FFormular;
  FormControlView.Items[0].ImageIndex := 0;
  FormControlView.Items[0].SelectedIndex := 0;
  EventEditor.ColWidths[0] := EventEditor.Width div 2;
  UndoStack := TPropChangeStack.Create;
  RedoStack := TPropChangeStack.Create;
  FCopyLst := TObjectList.Create(False);
  FMaxUndoSize:=1024;
  BorderHeight:=32;
  BorderWidth:=15;
  ReLoadConf;
end;

destructor TFormEditFrame.Destroy;
begin
  DeleteItem(FormControlView.Items[0]);
  FFuncList.Free;
  FCopyLst.Free;
  UndoStack.Free;
  RedoStack.Free;
  inherited;
end;

procedure TFormEditFrame.FormPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  n, i: integer;
  b: boolean;
begin
  FChangeProps := True;
  try
    b := False;
    if not (ssLeft in Shift) then
    begin
      FSelPoint := Point(-1, -1);
      if (Y >= (Sender as TControl).ClientHeight - 5) and
        (X >= (Sender as TControl).ClientWidth - 5) then
        (Sender as TControl).Cursor := crSizeNWSE
      else if (Y >= (Sender as TControl).ClientHeight - 5) then
        (Sender as TControl).Cursor := crSizeNS
      else if (X >= (Sender as TControl).ClientWidth - 5) then
        (Sender as TControl).Cursor := crSizeWE
      else
        (Sender as TControl).Cursor := crDefault;
    end
    else
    begin
      case MovingControl.Cursor of
        crSizeNWSE:
        begin
          MovingControl.Width :=
            X div (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
          MovingControl.Height :=
            Y div (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
          Moved := True;
          b := False;
          if FConf.UseHelpLines then
            with MovingControl do
              for n := 0 to Parent.ControlCount - 1 do
              begin
                if (Left + Width < Parent.Controls[n].Left +
                  Parent.Controls[n].Width + FConf.RasterSize) and
                  (Left + Width > Parent.Controls[n].Left +
                  Parent.Controls[n].Width - FConf.RasterSize) then
                begin
                  Width := Parent.Controls[n].Left + Parent.Controls[n].Width - Left;
                  b := True;
                end;
                if (Top + Height < Parent.Controls[n].Top +
                  Parent.Controls[n].Height + FConf.RasterSize) and
                  (Top + Height > Parent.Controls[n].Top +
                  Parent.Controls[n].Height - FConf.RasterSize) then
                begin
                  Height := Parent.Controls[n].Top + Parent.Controls[n].Height - Top;
                  b := True;
                end;
                if b then
                begin
                  FDrawLines := True;
                  Parent.Invalidate;
                  Exit;
                end;
                FDrawLines := False;
              end;
        end;
        crSizeNS:
        begin
          MovingControl.Height :=
            Y div (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
          Moved := True;
          if FConf.UseHelpLines then
            with MovingControl do
              for n := 0 to Parent.ControlCount - 1 do
              begin

                if (Top + Height < Parent.Controls[n].Top +
                  Parent.Controls[n].Height + FConf.RasterSize) and
                  (Top + Height > Parent.Controls[n].Top +
                  Parent.Controls[n].Height - FConf.RasterSize) then
                begin
                  Height := Parent.Controls[n].Top + Parent.Controls[n].Height - Top;
                  FDrawLines := True;
                  FFormular.Invalidate;
                  Exit;
                end;
                FDrawLines := False;
              end;
        end;
        crSizeWE:
        begin
          MovingControl.Width :=
            X div (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
          Moved := True;
          if FConf.UseHelpLines then
            with MovingControl do
              for n := 0 to Parent.ControlCount - 1 do
              begin
                if (Left + Width < Parent.Controls[n].Left +
                  Parent.Controls[n].Width + FConf.RasterSize) and
                  (Left + Width > Parent.Controls[n].Left +
                  Parent.Controls[n].Width - FConf.RasterSize) then
                begin
                  Width := Parent.Controls[n].Left + Parent.Controls[n].Width - Left;
                  FDrawLines := True;
                  FFormular.Invalidate;
                  Exit;
                end;
                FDrawLines := False;
              end;
        end;
        else
        begin
          if MovingControl <> FFormular then
          begin
            MovingControl.Left :=
              ((Sender as TControl).Parent.ScreenToClient(
              (Sender as TControl).ClientToScreen(Point(X, Y))).X - FMousePoint.X) div
              (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
            MovingControl.Top :=
              ((Sender as TControl).Parent.ScreenToClient(
              (Sender as TControl).ClientToScreen(Point(X, Y))).Y - FMousePoint.y) div
              (FConf.RasterSize div 2) * (FConf.RasterSize div 2);
            Moved := True;
            for i := 0 to FormControlView.Items.Count - 1 do
              if (i >= 0) and (i < FormControlView.Items.Count) and
                FormControlView.Items[i].Selected and
                (FormControlView.Items[i].Data <> Pointer(FFormular)) and
                (FormControlView.Items[i].Data <> Pointer(MovingControl)) then
                with TControl(FormControlView.Items[i].Data) do
                begin
                  Left := Left + (MovingControl.Left - FOldLeft);
                  Top := Top + (MovingControl.Top - FOldTop);
                end;
            FOldLeft := MovingControl.Left;
            FOldTop := MovingControl.Top;
            if FConf.UseHelpLines then
              with MovingControl as TControl do
                for n := 0 to Parent.ControlCount - 1 do
                begin
                  if (Left < Parent.Controls[n].Left + FConf.RasterSize) and
                    (Left > Parent.Controls[n].Left - FConf.RasterSize) then
                  begin
                    Left := Parent.Controls[n].Left;
                    b := True;
                  end
                  else if (Left + Width < Parent.Controls[n].Left +
                    Parent.Controls[n].Width + FConf.RasterSize) and
                    (Left + Width > Parent.Controls[n].Left +
                    Parent.Controls[n].Width - FConf.RasterSize) then
                  begin
                    Left := Parent.Controls[n].Left + Parent.Controls[n].Width - Width;
                    b := True;
                  end;
                  if (Top < Parent.Controls[n].Top + FConf.RasterSize) and
                    (Top > Parent.Controls[n].Top - FConf.RasterSize) then
                  begin
                    Top := Parent.Controls[n].Top;
                    b := True;
                  end
                  else if (Top + Height < Parent.Controls[n].Top +
                    Parent.Controls[n].Height + FConf.RasterSize) and
                    (Top + Height > Parent.Controls[n].Top +
                    Parent.Controls[n].Height - FConf.RasterSize) then
                  begin
                    Top := Parent.Controls[n].Top + Parent.Controls[n].Height - Height;
                    b := True;
                  end;
                  if b then
                  begin
                    FDrawLines := True;
                    Parent.Invalidate;
                    Exit;
                  end;
                  FDrawLines := False;
                end;
          end;
          if (ToolSelect.ItemIndex >= 0) and (Sender is TWinControl) then
          begin
            FSelPoint := FFormular.ScreenToClient(
              (Sender as TControl).ClientToScreen(Point(X, Y)));
            FFormular.Invalidate;
            Moved := True;
          end;
        end;
      end;
    end;
  finally
    FChangeProps := False;
  end;
end;

procedure TFormEditFrame.FormPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  d: TPoint;
  c: TControl;
  i: integer;
  cd: TChangeData;
begin
  if mbLeft = Button then
  begin
    if (ToolSelect.ItemIndex >= 0) then
    begin
      case ToolSelect.ItemIndex of
        0: c := CreateButton(FFormular);
        1: c := CreateCheckBox(FFormular);
        2: c := CreateEdit(FFormular);
        3: c := CreateLabel(FFormular);
      end;

      if (FSelPoint.X - FMousePoint.x >= 0) and (FSelPoint.y - FMousePoint.Y >= 0) then
      begin
        c.Width := FSelPoint.X - FMousePoint.x;
        c.Height := FSelPoint.y - FMousePoint.Y;
        FSelPoint := Point(-1, -1);
        c.Parent.Invalidate;
      end;
      ToolSelect.ItemIndex := -1;
      if Assigned(FOnVarChanged) then
        FOnVarChanged(Self);
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
    if Moved then
    begin
      if sizing then
      begin
        d.x := MovingControl.Width - FStartLeft;
        d.y := MovingControl.Height - FStartTop;
      end
      else
      begin
        d.x := MovingControl.Left - FStartLeft;
        d.y := MovingControl.Top - FStartTop;
      end;
      for i := 0 to FormControlView.Items.Count - 1 do
        if FormControlView.Items[i].Selected then
        begin
          c := TControl(FormControlView.Items[i].Data);
          cd.comp := c;
          if sizing then
          begin
            cd.Prop := 'Size';
            cd.OldVal := IntToStr(c.Width - d.x) + ':' + IntToStr(c.Height - d.y);
            cd.NewVal := IntToStr(c.Width) + ':' + IntToStr(c.Height);
          end
          else
          begin
            cd.Prop := 'Pos';
            cd.OldVal := IntToStr(c.Left - d.x) + ':' + IntToStr(c.Top - d.y);
            cd.NewVal := IntToStr(c.Left) + ':' + IntToStr(c.Top);
          end;
          UndoStack.Push(cd);
        end;

      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
    Moved := False;
    PositionPickerPanel.Show;
    FMousePoint := Point(-1, -1);
    FDrawLines := False;
    LoadControlData(TComponent(FormControlView.Selected.Data));
    FFormular.Invalidate;
  end;
end;

procedure TFormEditFrame.AddToVarlist(l: TVarList);
var
  i: integer;
begin
  l.Clear;
  for i := 0 to FormControlView.Items.Count - 1 do
  begin
    l.Add(VarInfo('$' + FormControlView.Items[i].Text, 0, i, FFileName));
  end;
  if FFormular.isMainForm then
    l.Add(VarInfo('$PerformClose', 0, 0, FFileName));
end;

procedure TFormEditFrame.FormPanelPaint(Sender: TObject);
var
  i, n: integer;
  c: TControl;
begin
  if FConf.UseRaster then
    for i := 0 to (Sender as TCustomControl).Width div FConf.RasterSize do
      for n := 0 to (Sender as TCustomControl).Height div FConf.RasterSize do
        (Sender as TCustomControl).Canvas.Pixels[i * FConf.RasterSize,
          n * FConf.RasterSize] :=
          clgray;
  if (FSelPoint.x >= 0) and (FSelPoint.y >= 0) then
  begin
    (Sender as TCustomControl).Canvas.Brush.Color := (Sender as TCustomControl).Color;
    (Sender as TCustomControl).Canvas.Clear;
    (Sender as TCustomControl).Canvas.Brush.Style := bsClear;
    (Sender as TCustomControl).Canvas.Pen.Style := psDash;
    (Sender as TCustomControl).Canvas.Pen.Color := clBlack;
    (Sender as TCustomControl).Canvas.Pen.Mode := pmNotXor;
    (Sender as TCustomControl).Canvas.Rectangle(FPanelMousePoint.X,
      FPanelMousePoint.Y, FSelPoint.x, FSelPoint.Y);
  end;
  if ((FMousePoint.x = -1) and (FMousePoint.y = -1)) or FDrawLines then
    for i := 0 to FormControlView.Items.Count - 1 do
      if FormControlView.Items[i].Selected then
      begin
        c := (Sender as TCustomControl).FindChildControl(
          TControl(FormControlView.Items[i].Data).Name);
        if Assigned(c) then
          with (Sender as TCustomControl).Canvas do
          begin
            if not FDrawLines then
            begin
              Pen.Style := psDash;
              Pen.Mode := pmCopy;
              Brush.Style := bsClear;
              Pen.Color := clBlack;
              Rectangle(c.Left - 1, c.Top - 1, c.Left + c.Width + 1,
                c.Top + c.Height + 1);
            end;
            Pen.Color := clHighlight;
            Pen.Style := psSolid;
            if FConf.UseHelpLines then
              for n := 0 to (Sender as TCustomControl).ControlCount - 1 do
              begin
                if (Sender as TCustomControl).Controls[n].Left = c.Left then
                  Line(c.Left, c.Top, c.Left,
                    (Sender as TCustomControl).Controls[n].Top);
                if (Sender as TCustomControl).Controls[n].top = c.Top then
                  Line(c.Left, c.Top,
                    (Sender as TCustomControl).Controls[n].Left, c.Top);
                if (Sender as TCustomControl).Controls[n].top +
                (Sender as TCustomControl).Controls[n].Height = c.Top + c.Height then
                  Line(c.Left, c.Top + c.Height,
                    (Sender as TCustomControl).Controls[n].Left,
                    c.Top + c.Height);
                if (Sender as TCustomControl).Controls[n].Left +
                (Sender as TCustomControl).Controls[n].Width = c.Left + c.Width then
                  Line(c.Left + c.Width, c.Top, c.Left + c.Width,
                    (Sender as TCustomControl).Controls[n].Top);
              end;

          end;
      end;
end;

procedure TFormEditFrame.OISplitterMoved(Sender: TObject);
begin
  if PropertyPanel.Align = alRight then
    PositionPickerPanel.Left := PropertyPanel.Left - PositionPickerPanel.Width - 8
  else
    ToolBoxPanel.Left := OISplitter.Left + OISplitter.Width + 16;
end;

procedure TFormEditFrame.PositionPickerMouseEnter(Sender: TObject);
begin
  PositionPickerPanel.Left := PositionPickerPanel.Left - 80;
  PositionPickerPanel.Width := PositionPickerPanel.Width + 80;
  PositionPickerPanel.Top := PositionPickerPanel.Top - 45;
  PositionPickerPanel.Height := PositionPickerPanel.Height + 45;
end;

procedure TFormEditFrame.PositionPickerMouseLeave(Sender: TObject);
begin
  PositionPickerPanel.Left := PositionPickerPanel.Left + 80;
  PositionPickerPanel.Width := PositionPickerPanel.Width - 80;
  PositionPickerPanel.Top := PositionPickerPanel.Top + 45;
  PositionPickerPanel.Height := PositionPickerPanel.Height - 45;
end;

procedure TFormEditFrame.PositionPickerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  fh, fw: double;
begin
  if ssLeft in Shift then
  begin
    FChangeProps := True;
    try
      fw := Screen.Width / PositionPicker.Width;
      fh := Screen.Height / PositionPicker.Height;
      FFormular.Left := Min(max(0, trunc(X * fw) - FFormular.Width div 2),
        Screen.Width - FFormular.Width);
      FFormular.Top := Min(max(0, trunc(Y * fH) - FFormular.Height div 2),
        Screen.Height - FFormular.Height);
      FormControlView.Select(FormControlView.Items[0]);
      PositionPicker.Invalidate;
      if Assigned(FOnChange) then
        FOnChange(Self);
    finally
      FChangeProps := False;
    end;
  end;
end;

procedure TFormEditFrame.PositionPickerPaint(Sender: TObject);
var
  fh, fw: double;
  px, py: integer;
  px2, py2: integer;
begin
  if not Assigned(FFormular) then
    exit;
  with PositionPicker.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FConf.TBCol;
    Pen.Style := psClear;
    Rectangle(0, 0, PositionPicker.Width, PositionPicker.Height);
    fw := PositionPicker.Width / Screen.Width;
    fh := PositionPicker.Height / Screen.Height;
    px := trunc(FFormular.Left * fw);
    py := trunc(FFormular.Top * fh);
    px2 := px + trunc(FFormular.Width * fw);
    py2 := py + trunc(FFormular.Height * fh);
    Brush.Color := clWindow;
    pen.Style := psSolid;
    pen.Color := clBlack;
    Rectangle(px, py, px2, py2);
  end;
end;

procedure TFormEditFrame.PropEditorEditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  aShow := (PropEditor.TIObject as Iau3Component).isProperty[aEditor.GetName];
end;

procedure TFormEditFrame.PropertyPanelResize(Sender: TObject);
begin
  PropertyPages.Height := (Sender as TControl).Height div 2;
end;

procedure TFormEditFrame.ToolboxHeaderPanelClick(Sender: TObject);
begin
  if ToolBoxPanel.Top > Parent.ClientHeight - ToolBoxPanel.Height then
    ToolBoxPanel.Top := Parent.ClientHeight - ToolBoxPanel.Height
  else
    ToolBoxPanel.Top := Parent.ClientHeight - ToolboxHeaderPanel.Height;
end;

procedure TFormEditFrame.ToolboxHeaderPanelMouseEnter(Sender: TObject);
begin
  ToolboxHeaderPanel.Color := clWhite;
end;

procedure TFormEditFrame.ToolboxHeaderPanelMouseLeave(Sender: TObject);
begin
  ToolboxHeaderPanel.Color := FConf.TBCol;
end;

procedure TFormEditFrame.FormPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  i: integer;
begin
  if Button = mbLeft then
  begin
    MovingControl := Sender as TControl;
    FMousePoint := Point(X, Y);
    FPanelMousePoint := FFormular.ScreenToClient(
      (Sender as TControl).ClientToScreen(Point(X, Y)));
    if FFormular.Cursor = crSizeNWSE then
      PositionPickerPanel.Hide;
    EditorScrollBox.Invalidate;
    FOldLeft := (Sender as TControl).Left;
    FOldTop := (Sender as TControl).Top;
    if (Sender as TControl).Cursor = crDefault then
    begin
      FStartLeft := (Sender as TControl).Left;
      FStartTop := (Sender as TControl).Top;
      sizing := False;
    end
    else
    begin
      FStartLeft := (Sender as TControl).Width;
      FStartTop := (Sender as TControl).Height;
      sizing := True;
    end;
    if not Assigned(ToolSelect.Selected) then
      for i := 0 to FormControlView.Items.Count - 1 do
        if FormControlView.Items[i].Data = Pointer(Sender) then
          if not FormControlView.Items[i].Selected then
          begin
            if not (ssShift in Shift) then
              FormControlView.ClearSelection;
            FormControlView.Items[i].Selected := True;
          end;
  end;
end;

procedure TFormEditFrame.FormControlViewChange(Sender: TObject; Node: TTreeNode);
var
  i: integer;
begin
  if not Assigned(Node) then
  begin
    if FormControlView.Items.Count > 0 then
      FormControlView.Select(FormControlView.Items[0]);
    exit;
  end;
  EventEditor.Clear;
  for i := 0 to FormControlView.Items.Count - 1 do
    if FormControlView.Items[i].Selected then
      LoadControlData(TControl(FormControlView.Items[i].Data));
  if Self.IsVisible then
    FormControlView.SetFocus;
  EditorScrollBox.Invalidate;
end;

procedure TFormEditFrame.EventEditorEditingDone(Sender: TObject);
var
  c: TComponent;
  s, v: string;
begin
  if not Assigned(FormControlView.Selected) then
    exit;
  s := EventEditor.Rows[EventEditor.Row][0];
  v := EventEditor.Values[s];
  c := TComponent(FormControlView.Selected.Data);
  (c as Iau3Component).Event[s] := v;
  if Assigned(FOnChange) then
    FOnChange(Self);
  LoadControlData(TComponent(c));
end;

procedure TFormEditFrame.EditorScrollBoxPaint(Sender: TObject);
var
  i: integer;
begin
  EditorScrollBox.Canvas.Brush.Color := (EditorScrollBox.Color);
  EditorScrollBox.Canvas.Brush.Style := bsSolid;
  EditorScrollBox.Canvas.Pen.Style := psClear;
  EditorScrollBox.Canvas.Rectangle(0, 0, EditorScrollBox.ClientWidth,
    EditorScrollBox.ClientHeight);
  if (FMousePoint.x = -1) and (FMousePoint.y = -1) then
    for i := 0 to FormControlView.Items.Count - 1 do
      if (FormControlView.Items[i].Selected) and
        (TControl(FormControlView.Items[i].Data) = FFormular) then
      begin
        EditorScrollBox.Canvas.Brush.Style := bsClear;
        EditorScrollBox.Canvas.Pen.Style := psDash;
        EditorScrollBox.Canvas.Pen.Mode := pmNotXor;
        EditorScrollBox.Canvas.Rectangle(FFormular.EditorLeft - 1,
          FFormular.EditorTop - 1,
          FFormular.EditorLeft + FFormular.Width + 1, FFormular.EditorTop +
          FFormular.Height + 1);
      end;
end;

procedure TFormEditFrame.EventEditorGetPickList(Sender: TObject;
  const KeyName: string; Values: TStrings);
begin
  Values.Add(Format('(%s)', [SNone]));
  Values.Add(Format('(%s...)', [SNew]));
  Values.AddStrings(FFuncList);
end;

procedure TFormEditFrame.EventEditorPickListSelect(Sender: TObject);
var
  s, v: string;
  i: integer;
begin
  s := EventEditor.Rows[EventEditor.Row][0];
  v := EventEditor.Rows[EventEditor.Row][1];
  if v = Format('(%s)', [SNone]) then
  begin
    EventEditor.Values[s] := '';
    Exit;
  end;
  if v = '' then
    Exit;
  if v = Format('(%s...)', [SNew]) then
  begin
    Sender:=Nil;
    v := FormControlView.Selected.Text + Copy(s, 3, Length(s));
    if StringsContain(FFuncList, v) then
    begin
      i := 1;
      while StringsContain(FFuncList, v + IntToStr(i)) do
        Inc(i);
      v := v + IntToStr(i);
    end;
    EventEditor.Values[s] := v;
  end;
    if Assigned(FEnterFunc) and (Sender=nil) then
      FEnterFunc(ChangeFileExt(FFileName, '.au3'), v, '', True);
end;

procedure TFormEditFrame.FormControlViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);

  function isValidName(s: string): boolean;
  var
    c: char;
  begin
    Result := Length(s) > 0;
    for c in s do
      if not (c in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) then
      begin
        Result := False;
        Break;
      end;
  end;

begin
  if not isValidName(s) then
  begin
    s := Node.Text;
    Exit;
  end;
  (TObject(Node.Data) as TComponent).Name := s;
  FormControlViewChange(Sender, Node);
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOnVarChanged) then
    FOnVarChanged(Self);
end;

procedure TFormEditFrame.DeleteItem(n: TTreeNode);
var
  tmpStack: TPropChangeStack;
begin
  if not Assigned(n) then
    Exit;
  while n.HasChildren do
    DeleteItem(n.GetFirstChild);
  if TObject(n.Data) <> FFormular then
  begin
    TObject(n.Data).Free;
    FormControlView.Items.Delete(n);
  end;

  tmpStack := TPropChangeStack.Create;
  try
    // Deleting all occurances in Undo
    while not UndoStack.IsEmpty() do
    begin
      if UndoStack.Top.comp <> TObject(n.Data) then
        tmpStack.Push(UndoStack.Top());
      UndoStack.Pop();
    end;
    while not tmpStack.IsEmpty() do
    begin
      UndoStack.Push(tmpStack.Top());
      tmpStack.Pop();
    end;
    // Deleting all occurances in Redo
    while not RedoStack.IsEmpty() do
    begin
      if RedoStack.Top.comp <> TObject(n.Data) then
        tmpStack.Push(RedoStack.Top());
      RedoStack.Pop();
    end;
    while not tmpStack.IsEmpty() do
    begin
      RedoStack.Push(tmpStack.Top());
      tmpStack.Pop();
    end;
  finally
    tmpStack.Free;
  end;

  FFormular.Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TFormEditFrame.SetMainForm(b: boolean; Silent: boolean = False);
begin
  FFormular.isMainForm := b;
  if Assigned(FOnChange) and not Silent then
    FOnChange(Self);
end;

procedure TFormEditFrame.FormControlViewKeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  i, tmp, n: integer;
  tmplst: TObjectList;
begin
  if (Key = 46) then
  begin
    for i := 0 to FormControlView.Items.Count - 1 do
      if FormControlView.Items[i].Selected then
        Application.QueueAsyncCall(TDataEvent(@DeleteItem),
          IntPtr(FormControlView.Items[i]));
    if Assigned(FOnVarChanged) then
      FOnVarChanged(Self);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else if (Key = Ord('C')) and (ssCtrl in Shift) then
  begin
    FCopyLst.Clear;
    for i := 0 to FormControlView.Items.Count - 1 do
      if FormControlView.Items[i].Selected then
        FCopyLst.Add(TObject(FormControlView.Items[i].Data));
  end
  else if Key=13 then
  begin
    if PropertyPages.PageIndex=0 then
      PropEditor.SetItemIndexAndFocus(PropEditor.ItemIndex)
    else
      EventEditor.SetFocus;
  end
  else if (Key = Ord('V')) and (ssCtrl in Shift) then
  begin
    tmplst := TObjectList.Create(False);
    try
      for i := 0 to FCopyLst.Count - 1 do
      begin
        tmp:=tmplst.Add(CopyControl(FCopyLst[i]));
        FCopyLst[i]:=tmplst[tmp];
      end;
      for i := 0 to tmplst.Count - 1 do
      begin
        for n := 0 to FormControlView.Items.Count - 1 do
          if FormControlView.Items[n].Data = Pointer(tmplst[i]) then
          begin
            FormControlView.Items[n].Selected := True;
            Break;
          end;
        (tmplst[i] as TControl).Left := (tmplst[i] as TControl).Left + 10;
        (tmplst[i] as TControl).Top := (tmplst[i] as TControl).Top + 10;
      end;
      for i := 0 to tmplst.Count do
        if tmplst[i] is TWinControl then
        begin
          (tmplst[i] as TWinControl).SetFocus;
          Break;
        end;
    finally
      tmplst.Free;
    end;
  end;
end;

procedure TFormEditFrame.FormPanelDblClick(Sender: TObject);
var
  s: string;
begin
  EventEditor.Row := 0;
  s := EventEditor.Rows[0][0];
  if EventEditor.Values[s] = '' then
    EventEditor.Values[s] := Format('(%s...)', [SNew]);
  EventEditorPickListSelect(nil);
end;

procedure TFormEditFrame.Save(p: string = '');
var
  sl: TStringList;
  i: integer;
  c: Iau3Component;
begin
  if p = '' then
    p := FFileName;
  sl := TStringList.Create;
  try
    sl.Text := FFormular.Getau3String(FFormular.Name);
    if p <> '' then
      sl.SaveToFile(p);
  finally
    sl.Free;
  end;
  FFileName := p;
end;

procedure TFormEditFrame.Load(p: string = '');

  function IsNumeric(s: string): boolean;
  var
    i, st: integer;
    cs: set of char;
  begin
    st := 1;
    Result := Length(s) > 0;
    if s[1] = '-' then
    begin
      st := 2;
      Result := Length(s) > 1;
    end;
    if pos('0x', s) = 1 then
    begin
      cs := ['0'..'9', 'A'..'F', 'a'..'f'];
      Inc(st, 2);
    end
    else
      cs := ['0'..'9'];
    for i := st to Length(s) do
      if not (s[i] in cs) then
      begin
        Result := False;
        Break;
      end;
  end;

  function ReadFunc(s: string; Params: TStringList): string;

    function ReadTok(s: string; out NewPos: integer): string;
    var
      len, depth: integer;
    begin
      len := 0;
      if s[1] = '"' then
      begin
        while (len + 2 < length(s)) and (s[2 + len] <> '"') do
          Inc(len);
        Result := Copy(s, 2, len);
        NewPos := len + 2;
        while not (s[NewPos] in [',', ')']) do
          Inc(NewPos);
      end
      else
      begin
        depth := 0;
        len := 0;
        while (len < length(s) - 1) and not ((depth = 0) and
            (s[1 + len] in [',', ')'])) do
        begin
          if (s[1 + len] = '(') then
            Inc(depth)
          else if (s[1 + len] = ')') then
            Dec(depth);
          Inc(len);
        end;
        Result := Copy(s, 1, len);
        NewPos := len + 1;
        while not (s[NewPos] in [',', ')']) do
          Inc(NewPos);
      end;
    end;

  var
    i: integer;
  begin
    Result := '';
    if (Pos('(', s) = 0) or (Pos(')', s) = 0) or (Params = nil) then
      exit;
    Params.Clear;
    s := Trim(s);
    i := 1;
    while (i <= length(s)) and (s[i] <> '(') do
      Inc(i);
    Result := Copy(s, 1, i - 1);
    Delete(s, 1, Pos('(', s));

    while not ((ReadTok(s, i) = ')') or (ReadTok(s, i) = '')) do
    begin
      Params.Add(ReadTok(s, i));
      while s[i] in [' ', #9, ','] do
        Inc(i);
      Delete(s, 1, i - 1);
    end;
  end;

  function ReadVar(s: string): string;
  var
    i: integer;
  begin
    i := 2;
    Result := '';
    if (Length(s) = 0) or (s[1] <> '$') then
      exit;
    while (i <= Length(s)) and (s[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(i);
    if s[i] = '[' then
      exit;
    Result := Copy(s, 2, i - 2);
  end;

var
  Lines: TStringList;
  VarName, FuncName: string;
  currParent: TTreeNode;
  FuncParams: TStringList;
  i, curr: integer;
  FormFound: boolean;
  a: Iau3Component;
  c: TControl;
  idx: integer;
  rm: TResizeModes;
begin
  FChangeProps := True;
  try
    currParent := nil;
    rm := [];
    curr := 1;
    DeleteItem(FormControlView.Items[0]);
    FormFound := False;
    if p = '' then
      p := FFileName;
    if not FileExists(p) then
      exit;
    Lines := TStringList.Create;
    FuncParams := TStringList.Create;
    try
      Lines.LoadFromFile(p);
      for i := 0 to Lines.Count - 1 do
      begin
        // Control Creation
        if (Length(Lines[i]) > 0) and (Lines[i][1] = '$') then
        begin
          VarName := ReadVar(Lines[i]);
          FuncName := Lines[i];
          Delete(FuncName, 1, Pos('=', FuncName));
          FuncName := LowerCase(ReadFunc(FuncName, FuncParams));
          if (FuncName = '') or (FuncParams.Count = 0) then
            Continue;
          if FuncName = 'guicreate' then
          begin
            // Load Form
            // Syntax Check
            if FormFound or (FuncParams.Count <> 7) or not
              (IsNumeric(FuncParams[1]) and IsNumeric(FuncParams[2]) and
              IsNumeric(FuncParams[3]) and IsNumeric(FuncParams[4]) and
              IsNumeric(FuncParams[5]) and IsNumeric(FuncParams[6])) then
              Continue;
            // Read Data
            FFormular.Name := VarName;
            FormControlView.Items[0].Text := FFormular.Name;
            FFormular.Text := FuncParams[0];
            FFormular.Left := StrToInt(FuncParams[3]);
            FFormular.Top := StrToInt(FuncParams[4]);
            FFormular.Width := StrToInt(FuncParams[1]) - BorderWidth;
            FFormular.Height := StrToInt(FuncParams[2]) - BorderHeight;
            FFormular.Style := TWindowStyles(StrToInt(FuncParams[5]) shr 16);
            FFormular.StyleEx := TWindowExStyles(StrToInt(FuncParams[6]));
            FormFound := True;
            FFormular.Resize := rm;
            FFormular.Visible := True;
            FFormular.Enabled := True;
            currParent := FormControlView.Items[0];
          end
          else if FuncName = 'guictrlcreatebutton' then
          begin
            // Syntax Check
            if (FuncParams.Count <> 7) or not Assigned(currParent) or
              not (IsNumeric(FuncParams[1]) and IsNumeric(FuncParams[2]) and
              IsNumeric(FuncParams[3]) and IsNumeric(FuncParams[4]) and
              IsNumeric(FuncParams[5]) and IsNumeric(FuncParams[6])) then
              Continue;
            // Read Data
            c := CreateButton(TObject(currParent.Data) as TWinControl);
            c.Name := VarName;
            FormControlView.Items[curr].Text := VarName;
            c.Caption := FuncParams[0];
            c.Left := StrToInt(FuncParams[1]);
            c.Top := StrToInt(FuncParams[2]);
            c.Width := StrToInt(FuncParams[3]);
            c.Height := StrToInt(FuncParams[4]);
            (c as Tau3Button).CompleteStyle := StrToInt(FuncParams[5]);
            (c as Tau3Button).ComponentProp['StyleEx'] := FuncParams[6];
            (c as Tau3Button).TabOrder := curr;
            Inc(curr);
          end
          else if FuncName = 'guictrlcreatecheckbox' then
          begin
            // Syntax Check
            if (FuncParams.Count <> 7) or not Assigned(currParent) or
              not (IsNumeric(FuncParams[1]) and IsNumeric(FuncParams[2]) and
              IsNumeric(FuncParams[3]) and IsNumeric(FuncParams[4]) and
              IsNumeric(FuncParams[5]) and IsNumeric(FuncParams[6])) then
              Continue;
            // Read Data
            c := CreateCheckBox(TObject(currParent.Data) as TWinControl);
            c.Name := VarName;
            FormControlView.Items[curr].Text := VarName;
            c.Caption := FuncParams[0];
            c.Left := StrToInt(FuncParams[1]);
            c.Top := StrToInt(FuncParams[2]);
            c.Width := StrToInt(FuncParams[3]);
            c.Height := StrToInt(FuncParams[4]);
            (c as Tau3Checkbox).CompleteStyle := StrToInt(FuncParams[5]);
            (c as Tau3Checkbox).ComponentProp['StyleEx'] := FuncParams[6];
            (c as Tau3Checkbox).TabOrder := curr;
            Inc(curr);
          end
          else if FuncName = 'guictrlcreatelabel' then
          begin
            // Syntax Check
            if (FuncParams.Count <> 7) or not Assigned(currParent) or
              not (IsNumeric(FuncParams[1]) and IsNumeric(FuncParams[2]) and
              IsNumeric(FuncParams[3]) and IsNumeric(FuncParams[4]) and
              IsNumeric(FuncParams[5]) and IsNumeric(FuncParams[6])) then
              Continue;
            // Read Data
            c := CreateLabel(TObject(currParent.Data) as TWinControl);
            c.Name := VarName;
            FormControlView.Items[curr].Text := VarName;
            c.Caption := FuncParams[0];
            c.Left := StrToInt(FuncParams[1]);
            c.Top := StrToInt(FuncParams[2]);
            c.Width := StrToInt(FuncParams[3]);
            c.Height := StrToInt(FuncParams[4]);
            (c as Tau3Label).ComponentProp['Style'] := FuncParams[5];
            (c as Tau3Label).ComponentProp['StyleEx'] := FuncParams[6];
            (c as Tau3Label).TabOrder := curr;
            Inc(curr);
          end
          else if FuncName = 'guictrlcreateinput' then
          begin
            // Syntax Check
            if (FuncParams.Count <> 7) or not Assigned(currParent) or
              not (IsNumeric(FuncParams[1]) and IsNumeric(FuncParams[2]) and
              IsNumeric(FuncParams[3]) and IsNumeric(FuncParams[4]) and
              IsNumeric(FuncParams[5]) and IsNumeric(FuncParams[6])) then
              Continue;
            // Read Data
            c := CreateEdit(TObject(currParent.Data) as TWinControl);
            c.Name := VarName;
            FormControlView.Items[curr].Text := VarName;
            (c as Tau3Edit).Text := FuncParams[0];
            c.Left := StrToInt(FuncParams[1]);
            c.Top := StrToInt(FuncParams[2]);
            c.Width := StrToInt(FuncParams[3]);
            c.Height := StrToInt(FuncParams[4]);
            (c as Tau3Edit).CompleteStyle := StrToInt(FuncParams[5]);
            (c as Tau3Edit).ComponentProp['StyleEx'] := FuncParams[6];
            (c as Tau3Edit).TabOrder := curr;
            Inc(curr);
          end;
        end
        // Resize Modes
        else if isEnd(Lines[i], 'opt') then
        begin
          FuncName := ReadFunc(Lines[i], FuncParams);
          // Syntax Check
          if not ((LowerCase(FuncParams[0]) = 'guiresizemode') and
            IsNumeric(FuncParams[1])) then
            Continue;
          rm := TResizeModes(StrToInt(FuncParams[1]));
        end
        else if FormFound then
          // GUI Event
          if isEnd(Lines[i], 'guisetonevent') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 3) and IsNumeric(FuncParams[0]) and
              (Length(FuncParams[2]) > 0) and (FuncParams[2][1] = '$') and
              (StrToInt(FuncParams[0]) <> GUI_EVENT_CLOSE)) then
              Continue;
            FFormular.Event[GUIEventToString(StrToInt(FuncParams[0]))] :=
              FuncParams[1];
          end
          // Control Event
          else if isEnd(Lines[i], 'guictrlsetonevent') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 2) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$')) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            a.Events.ValueFromIndex[0] := FuncParams[1];
          end
          // Form Close Event
          else if isEnd(Lines[i], 'func') then
          begin
            idx := i + 1;
            FuncName := Lines[idx];
            if isEnd(Lines[idx], 'if') then
              Inc(idx);
            FuncName := ReadFunc(Lines[idx], FuncParams);
            FFormular.Event['onClose'] := FuncName;
          end
          // Form Color
          else if isEnd(Lines[i], 'guisetbkcolor') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count > 0) and IsNumeric(FuncParams[0])) then
              Continue;
            FFormular.Color := AuColToColor(FuncParams[0]);
          end
          // Form Cursor
          else if isEnd(Lines[i], 'guisetcursor') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 3) and IsNumeric(FuncParams[0])) then
              Continue;
            FFormular.CursorIcon := TAU3Cursor(StrToInt(FuncParams[0]));
          end
          // Form Font
          else if isEnd(Lines[i], 'guisetfont') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 4) and IsNumeric(FuncParams[0]) and
              IsNumeric(FuncParams[1]) and IsNumeric(FuncParams[2])) then
              Continue;
            SetFontString(FFormular.Font, ExtractBetween(Lines[i], '(', ')'));
          end
          // Form Icon
          else if isEnd(Lines[i], 'guiseticon') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 3) and IsNumeric(FuncParams[1])) then
              Continue;
            FFormular.Icon := FuncParams[0];
            FFormular.IconID := StrToInt(FuncParams[1]);
          end
          // Form State
          else if isEnd(Lines[i], 'guisetstate') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not (FuncParams.Count = 1) then
              Continue;
            if FuncParams[0] = '@SW_DISABLE' then
              FFormular.Enabled := False
            else if FuncParams[0] = '@SW_HIDE' then
              FFormular.Visible := False
            else
            begin
              FFormular.Visible := True;
              FFormular.Enabled := True;
            end;
          end
          // Control Font
          else if isEnd(Lines[i], 'guictrlsetfont') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 5) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$') and IsNumeric(FuncParams[1]) and
              IsNumeric(FuncParams[2]) and IsNumeric(FuncParams[3])) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            a.ComponentProp['Font'] :=
              Format('%s, %s, %s, "%s"', [FuncParams[1], FuncParams[2],
              FuncParams[3], FuncParams[4]]);
          end
          // Control Resizing
          else if isEnd(Lines[i], 'guictrlsetresizing') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 2) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$') and IsNumeric(FuncParams[1])) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            a.ComponentProp['Resizing'] := FuncParams[1];
          end
          // Control Cursor
          else if isEnd(Lines[i], 'guictrlsetcursor') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 2) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$') and IsNumeric(FuncParams[1])) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            a.ComponentProp['cursor'] := FuncParams[1];
          end
          // Control Color
          else if isEnd(Lines[i], 'guictrlsetbkcolor') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 2) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$') and IsNumeric(FuncParams[1])) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            a.ComponentProp['Color'] := IntToStr(AuColToColor(FuncParams[1]));
          end
          // Control Hint
          else if isEnd(Lines[i], 'guictrlsettip') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 2) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$')) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            a.ComponentProp['Hint'] := FuncParams[1];
          end
          // Control State
          else if isEnd(Lines[i], 'guictrlsetstate') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 2) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$') and IsNumeric(FuncParams[1])) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            if FuncParams[1] = '32' then
              a.ComponentProp['Visible'] := 'False'
            else if FuncParams[1] = '128' then
              a.ComponentProp['Enabled'] := 'False'
            else if FuncParams[1] = '1' then
              a.ComponentProp['Checked'] := 'True';
          end
          // Control Limit
          else if isEnd(Lines[i], 'guictrlsetlimit') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 2) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$') and IsNumeric(FuncParams[1])) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            a.ComponentProp['limit'] := FuncParams[1];
          end
          // Control Font Color
          else if isEnd(Lines[i], 'guictrlsetcolor') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 2) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$') and IsNumeric(FuncParams[1])) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            a.ComponentProp['FontColor'] := IntToStr(AuColToColor(FuncParams[1]));
          end
          // Control Image
          else if isEnd(Lines[i], 'guictrlsetimage') then
          begin
            FuncName := ReadFunc(Lines[i], FuncParams);
            // Syntax Check
            if not ((FuncParams.Count = 2) and (Length(FuncParams[0]) > 0) and
              (FuncParams[0][1] = '$')) then
              Continue;
            idx := FindControl(Copy(FuncParams[0], 2, length(FuncParams[0])));
            if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              Continue;
            a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
            a.ComponentProp['Picture'] := FuncParams[1];
          end
          // Hotkeys
          else if IsEnd(Lines[i], 'dim') then
          begin
            FuncName := Copy(Lines[i], Pos('=', Lines[i]) + 1, Length(Lines[i]));
            FuncName := Copy(FuncName, Pos('[', FuncName) + 1, Length(FuncName));
            while (Length(FuncName) > 1) do
            begin
              FuncParams.Clear;
              FuncParams.CommaText := ExtractBetween(FuncName, '[', ']');
              idx := FindControl(Copy(FuncParams[1], 2, length(FuncParams[1])));
              if (idx < 0) or (idx > FormControlView.Items.Count - 1) then
              begin
                Delete(FuncName, 1, Pos(']', FuncName));
                Continue;
              end;
              a := TObject(FormControlView.Items[idx].Data) as Iau3Component;
              a.ComponentProp['HotKey'] := AU3KeyToHotKey(FuncParams[0]);
              Delete(FuncName, 1, Pos(']', FuncName));
            end;
          end;
      end;
    finally
      FuncParams.Free;
      Lines.Free;
    end;
    FFileName := p;
    if Assigned(FOnVarChanged) then
      FOnVarChanged(Self);
    Self.Parent.Caption := ExtractFileName(p);
    FormControlView.Select(FormControlView.Items[0]);
    PositionPicker.Invalidate;
  finally
    FChangeProps := False;
  end;
end;

end.
