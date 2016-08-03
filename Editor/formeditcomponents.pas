unit FormEditComponents;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, ValEdit,
  LCLIntf, LCLType;

type
  TAU3Cursor =
    (craHAND,
    craAPPSTARTING,
    craARROW,
    cracraOSS,
    craHELP,
    craIBEAM,
    craICON,
    craNO,
    craSIZE,
    craSIZEALL,
    craSIZENESW,
    craSIZENS,
    craSIZENWSE,
    craSIZEWE,
    craUPARROW,
    craWAIT);

  TResizeMode = (
    rzDOCKAUTO,
    rzDOCKLEFT,
    rzDOCKRIGHT,
    rzDOCKHCENTER,
    rzFiller6,
    rzDOCKTOP,
    rzDOCKBOTTOM,
    rzDOCKVCENTER,
    rzDOCKWIDTH,
    rzDOCKHEIGHT);
  TResizeModes = set of TResizeMode;

  TWindowStyle = (
    WS_MAXIMIZEBOX,
    WS_MINIMIZEBOX,
    WS_SIZEBOX,
    WS_SYSMENU,
    WS_HSCROLL,
    WS_VSCROLL,
    WS_DLGFRAME,
    WS_BORDER,
    WS_MAXIMIZE,
    WS_CLIPCHILDREN,
    WS_CLIPSIBLINGS,
    WS_DISABLED,
    WS_VISIBLE,
    WS_MINIMIZE,
    WS_CHILD,
    WS_POPUP);
  TWindowStyles = set of TWindowStyle;

  TWindowExStyle = (
    WS_EX_DLGMODALFRAME,
    Filler1,
    WS_EX_NOPARENTNOTIFY,
    WS_EX_TOPMOST,
    WS_EX_ACCEPTFILES,
    WS_EX_TRANSPARENT,
    WS_EX_MDICHILD,
    WS_EX_TOOLWINDOW,
    WS_EX_WINDOWEDGE,
    WS_EX_CLIENTEDGE,
    WS_EX_CONTEXTHELP,
    Filler2,
    WS_EX_RIGHT,
    WS_EX_RTLREADING,
    WS_EX_LEFTSCROLLBAR,
    Filler3,
    WS_EX_CONTROLPARENT,
    WS_EX_STATICEDGE,
    WS_EX_APPWINDOW
    );
  TWindowExStyles = set of TWindowExStyle;

  TEditStyle = (
    ES_CENTER,
    ES_RIGHT,
    ES_MULTILINE,
    ES_UPPERCASE,
    ES_LOWERCASE,
    ES_PASSWORD,
    ES_AUTOVSCROLL,
    ES_AUTOHSCROLL,
    ES_NOHIDESEL,
    Filler4,
    ES_OEMCONVERT,
    ES_READONLY,
    ES_WANTRETURN,
    ES_NUMBER
    );
  TEditStyles = set of TEditStyle;

  TButtonStyle = (BS_DEFPUSHBUTTON,
    BS_CHECKBOX,
    BS_RADIOBUTTON,
    BS_USERBUTTON,
    Filler5,
    BS_LEFTTEXT,
    BS_ICON,
    BS_BITMAP,
    BS_LEFT,
    BS_RIGHT,
    BS_TOP,
    BS_BOTTOM,
    BS_PUSHLIKE,
    BS_MULTILINE,
    BS_NOTIFY,
    BS_FLAT
    );
  TButtonStyles = set of TButtonStyle;

  TStaticStyle = (
    SS_CENTER,
    SS_RIGHT,
    SS_BLACKRECT,
    SS_GRAYFRAME,
    SS_ETCHEDHORZ,
    Filler6,
    Filler7,
    SS_NOPREFIX,
    SS_NOTIFY,
    SS_CENTERIMAGE,
    SS_RIGHTJUST,
    SS_REALSIZEIMAGE,
    SS_SUNKEN
    );
  TStaticStyles = set of TStaticStyle;

  TPropertyChangeEvent = procedure(Sender: TObject;
    PropName, PropVal: string) of object;

  Iau3Component = interface
    ['{DE4489A7-9015-405B-8123-AF253975EBA0}']
    procedure CopyTo(c: TControl);
    procedure FillEvents(g: TValueListEditor);
    function Getau3String(FormName: string): string;

    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(prop: string): string;
    procedure SetProp(prop, val: string);
    function GetEvents: TStringList;
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);


    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read GetEvents;
    property OnChangeProp: TPropertyChangeEvent
      read GetOnChangeProp write SetOnChangeProp;
  end;

  Tau3Form = class(TCustomPanel, Iau3Component)
  private
    FIsMainForm: boolean;
    FStyle: cardinal;
    FStyleEx: cardinal;
    FEvents: TStringList;
    FLeft, FTop: integer;
    FOnChangeProp: TPropertyChangeEvent;
    FCaption: string;
    FEnabled: boolean;
    FVisible: boolean;
    FIcon: TFilename;
    FIconID: integer;
    FMyCursor: TAU3Cursor;
    FOnChangeCaption: TNotifyEvent;
    FResize: cardinal;
    function GetEditorTop: integer;
    function GetEditorLeft: integer;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetStyleEX(val: TWindowExStyles);

    procedure SetFormEnabled(Value: boolean);
    procedure SetFormCursor(Value: TAU3Cursor);
    procedure SetColor(Value: TColor); override;
    procedure SetFont(f: TFont);
    procedure SetFormVisible(Value: boolean);
    procedure SetIcon(s: string);
    procedure SetIconID(i: integer);
    procedure SetIsMainForm(b: boolean);
    procedure SetResize(val: TResizeModes);

    function GetFont: TFont;
    function GetColor: TColor;

    function GetStyle: TWindowStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetResize: TResizeModes;
    procedure Paint; override;
  public
    procedure SetFormPos(x, y: integer);
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function Getau3String(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);

    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
    property isMainForm: boolean read FIsMainForm write SetIsMainForm;
  published
    property EditorTop: integer read GetEditorTop;
    property EditorLeft: integer read GetEditorLeft;
    property Name;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property Text: string read FCaption write SetText;
    property Caption: string read FCaption write SetText;
    property OnChangeCaption: TNotifyEvent read FOnChangeCaption write FOnChangeCaption;
    property Enabled: boolean read FEnabled write SetFormEnabled;
    property Color: TColor read GetColor write SetColor;
    property Font: TFont read GetFont write SetFont;
    property CursorIcon: TAU3Cursor read FMyCursor write SetFormCursor;
    property Visible: boolean read FVisible write SetFormVisible;
    property Icon: string read FIcon write SetIcon;
    property IconID: integer read FIconID write SetIconID;
    property Resize: TResizeModes read GetResize write SetResize;

    property OnClick;
    property OnEnter;
    property OnExit;
    property Anchors;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

  { Tau3Edit }

  Tau3Edit = class(TCustomEdit, Iau3Component)
  private
    FEnabled: boolean;
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FOnChangeProp: TPropertyChangeEvent;
    FCursorIcon: TAU3Cursor;
    FisEnabled: boolean;
    FTabOrder: integer;
    FIsVisible: boolean;
    FResizing: TResizeModes;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetEditStyle(val: TEditStyles);
    procedure SetStyleEx(val: TWindowExStyles);

    procedure SetColor(Value: TColor); override;
    procedure SetCursorIcon(c: TAU3Cursor);
    procedure SetisEnabled(b: boolean);
    procedure SetFont(f: TFont);
    procedure SetMaxLen(l: integer);
    procedure SetHint(const Value: TTranslateString); override;
    procedure SetTabOrder(i: integer);
    procedure SetisVisible(b: boolean);
    procedure SetResizing(b: TResizeModes);

    function GetFont: TFont;
    function GetMaxLen: integer;

    function GetStyle: TWindowStyles;
    function GetEditStyle: TEditStyles;
    function GetStyleEx: TWindowExStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetText: string;
  public
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(prop: string): string;
    procedure SetProp(prop, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function Getau3String(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);

    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published

    property Name;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property EditStyle: TEditStyles read GetEditStyle write SetEditStyle;
    property CompleteStyle: cardinal read FStyle write FStyle;

    property MaxLength: integer read GetMaxLen write SetMaxLen;
    property CursorIcon: TAU3Cursor read FCursorIcon write SetCursorIcon;
    property Enabled: boolean read FEnabled write SetisEnabled;
    property Font: TFont read GetFont write SetFont;
    property Resizing: TResizeModes read FResizing write SetResizing;
    property Visible: boolean read FIsVisible write SetisVisible;
    property TabOrder: integer read FTabOrder write SetTabOrder;

    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property HideSelection;
    property NumbersOnly;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property Text: string read GetText write SetText;
    property Caption: string read GetText write SetText;
    property TextHint;
    property TextHintFontColor;
    property TextHintFontStyle;
  end;

  Tau3Button = class(TCustomButton, Iau3Component)
  private
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FLastClick: cardinal;
    FOnChangeProp: TPropertyChangeEvent;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetButtonStyle(val: TButtonStyles);
    procedure SetStyleEx(val: TWindowExStyles);
    function GetStyle: TWindowStyles;
    function GetButtonStyle: TButtonStyles;
    function GetStyleEx: TWindowExStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetText: string;
  public
    procedure Click; override;
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function Getau3String(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);

    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published
    property Name;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Text: string read GetText write SetText;
    property Caption: string read GetText write SetText;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property ButtonStyle: TButtonStyles read GetButtonStyle write SetButtonStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property CompleteStyle: cardinal read FStyle write FStyle;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel;
    property Color;
    property Constraints;
    property Default;
    property OnDblClick;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

  Tau3Checkbox = class(TCustomCheckBox, Iau3Component)
  private
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FOnChangeProp: TPropertyChangeEvent;
  protected
    procedure Click; override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetButtonStyle(val: TButtonStyles);
    procedure SetStyleEx(val: TWindowExStyles);
    function GetStyle: TWindowStyles;
    function GetButtonStyle: TButtonStyles;
    function GetStyleEx: TWindowExStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetText: string;
  public
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function Getau3String(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);

    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published
    property Name;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property ButtonStyle: TButtonStyles read GetButtonStyle write SetButtonStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property CompleteStyle: cardinal read FStyle write FStyle;
    property Text: string read GetText write SetText;
    property Caption: string read GetText write SetText;
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BidiMode;
    property BorderSpacing;
    property Checked;
    property OnDblClick;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ParentBidiMode;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop default True;
    property Visible;
  end;

  Tau3Label = class(TCustomControl, Iau3Component)
  private
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FOnChangeProp: TPropertyChangeEvent;
    FCaption: string;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetLeft(Val: integer);
    procedure SetTop(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetStaticStyle(val: TStaticStyles);
    procedure SetStyleEx(val: TWindowExStyles);
    function GetStyle: TWindowStyles;
    function GetStaticStyle: TStaticStyles;
    function GetStyleEx: TWindowExStyles;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure Paint; override;
  public
    function GetOnChangeProp: TPropertyChangeEvent;
    procedure SetOnChangeProp(a: TPropertyChangeEvent);
    function GetEvent(e: string): string;
    procedure SetEvent(e, val: string);
    function CheckProperty(prop: string): boolean;
    function GetProp(p: string): string;
    procedure SetProp(p, val: string);
    function GetEvents: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyTo(c: TControl);
    function Getau3String(FormName: string): string;
    procedure FillEvents(g: TValueListEditor);

    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published
    property Name;
    property Left: integer read GetLeft write SetLeft;
    property Y: integer read GetTop write SetTop;
    property X: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property StaticStyle: TStaticStyles read GetStaticStyle write SetStaticStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property CompleteStyle: cardinal read FStyle write FStyle;
    property Text: string read FCaption write SetText;
    property Caption: string read FCaption write SetText;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

const
  GUI_EVENT_NONE = 0;
  GUI_EVENT_CLOSE = -3;
  GUI_EVENT_MINIMIZE = -4;
  GUI_EVENT_RESTORE = -5;
  GUI_EVENT_MAXIMIZE = -6;
  GUI_EVENT_PRIMARYDOWN = -7;
  GUI_EVENT_PRIMARYUP = -8;
  GUI_EVENT_SECONDARYDOWN = -9;
  GUI_EVENT_SECONDARYUP = -10;
  GUI_EVENT_MOUSEMOVE = -11;
  GUI_EVENT_RESIZED = -12;
  GUI_EVENT_DROPPED = -13;

implementation

function GetFontString(f: TFont): string;

  function GetWeight: integer;
  begin
    if f.Bold then
      Result := 700
    else
      Result := 400;
  end;

  function GetAttr: integer;
  begin
    Result := 0;
    if f.Italic then
      Result := Result or 2;
    if f.Underline then
      Result := Result or 4;
    if f.StrikeThrough then
      Result := Result or 8;
  end;

begin
  Result := Format('%d,%d,%d,"%s"', [f.Size, GetWeight, GetAttr, f.Name]);
end;

procedure SetFontString(f: TFont; s: string);
begin

end;

function isNumeric(s: string): boolean;
var
  c: char;
begin
  Result := Length(s) > 0;
  for c in s do
    if not (c in ['0'..'9']) then
    begin
      Result := False;
      Break;
    end;
end;

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

{ Form }

procedure Tau3Form.SetFormPos(x, y: integer);
begin
  inherited Left := x;
  inherited Top := y;
end;

procedure Tau3Form.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(0, 0, Width, Height);
  Canvas.DrawFocusRect(Rect(0, 0, Width, Height));
  inherited;
end;

function Tau3Form.GetEditorTop: integer;
begin
  Result := inherited Top;
end;

function Tau3Form.GetEditorLeft: integer;
begin
  Result := inherited Left;
end;

function Tau3Form.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (Pos('ws_', prop) = 1) or (Pos('rz', prop) = 1) or
    (prop = 'enabled') or (prop = 'color') or (prop = 'cursoricon') or
    (prop = 'font') or (prop = 'icon') or (prop = 'resizing') or
    (prop = 'visible') or (prop = 'iconid') or (prop = 'styleex') or (prop = 'resize');
end;

function Tau3Form.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3Form.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  inherited Caption := '';
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure Tau3Form.SetLeft(Val: integer);
begin
  FLeft := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure Tau3Form.SetTop(Val: integer);
begin
  FTop := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure Tau3Form.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure Tau3Form.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function Tau3Form.GetLeft: integer;
begin
  Result := FLeft;
end;

function Tau3Form.GetTop: integer;
begin
  Result := FTop;
end;

function Tau3Form.GetWidth: integer;
begin
  Result := inherited Width;
end;

function Tau3Form.GetHeight: integer;
begin
  Result := inherited Height;
end;

function Tau3Form.GetResize: TResizeModes;
begin
  Result := TResizeModes(FResize);
end;

procedure Tau3Form.SetFormEnabled(Value: boolean);
begin
  FEnabled := Value;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Enabled', BoolToStr(Value, 'True', 'False'));
end;

procedure Tau3Form.SetFormCursor(Value: TAU3Cursor);
begin
  FMyCursor := Value;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Cursor', IntToStr(Ord(Value)));
end;

procedure Tau3Form.SetIcon(s: string);
begin
  FIcon := s;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Icon', s);
end;

procedure Tau3Form.SetIconID(i: integer);
begin
  FIconID := i;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'IconID', IntToStr(i));
end;

procedure Tau3Form.SetIsMainForm(b: boolean);
begin
  FIsMainForm := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, '', '');
end;

procedure Tau3Form.SetResize(val: TResizeModes);
begin
  FResize := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Resize', IntToStr(cardinal(val)));
end;

procedure Tau3Form.SetColor(Value: TColor);
begin
  inherited;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Color', IntToStr(Ord(Value)));
end;

procedure Tau3Form.SetFont(f: TFont);
begin
  inherited Font := f;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Font', GetFontString(f));
end;

procedure Tau3Form.SetFormVisible(Value: boolean);
begin
  FVisible := Value;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Visible', BoolToStr(Value, 'True', 'False'));
end;

function Tau3Form.GetFont: TFont;
begin
  Result := inherited Font;
end;

function Tau3Form.GetColor: TColor;
begin
  Result := inherited Color;
end;

procedure Tau3Form.SetStyle(val: TWindowStyles);
begin
  FStyle := DWord(val) shl 16;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(FStyle));
end;

procedure Tau3Form.SetStyleEX(val: TWindowExStyles);
begin
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)));
end;

function Tau3Form.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function Tau3Form.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure Tau3Form.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
end;

function Tau3Form.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Caption
  else if p = 'x' then
    Result := IntToStr(Left)
  else if p = 'y' then
    Result := IntToStr(Top)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(FStyle)
  else if p = 'styleex' then
    Result := IntToStr(FStyleEx)
  else if p = 'color' then
    Result := IntToStr(Color)
  else if p = 'cursor' then
    Result := IntToStr(Ord(FMyCursor))
  else if p = 'enabled' then
    Result := BoolToStr(FEnabled, 'True', 'False')
  else if p = 'icon' then
    Result := IntToStr(Height)
  else if p = 'iconid' then
    Result := IntToStr(Height)
  else if p = 'font' then
    Result := GetFontString(Font)
  else if p = 'visible' then
    Result := BoolToStr(FVisible, 'True', 'False')
  else if p = 'resize' then
    Result := IntToStr(FResize);
end;

procedure Tau3Form.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    FStyle := StrToInt64(val)
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEx := StrToInt(val)
  else if (p = 'color') and isNumeric(val) then
    inherited Color := StrToInt(val)
  else if (p = 'cursor') and isNumeric(val) then
    FMyCursor := TAU3Cursor(StrToInt(val))
  else if (p = 'resize') and isNumeric(val) then
    FResize := StrToInt(val)
  else if (p = 'enabled') then
    FEnabled := val = 'True'
  else if (p = 'icon') then
    FIcon := val
  else if (p = 'iconid') and isNumeric(val) then
    FIconID := StrToInt(val)
  else if (p = 'font') then
    SetFontString(Font, val)
  else if (p = 'visible') then
    FVisible := val = 'True';
end;

function Tau3Form.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure Tau3Form.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

constructor Tau3Form.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  Height := 312;
  Width := 386;
  inherited Name := 'Form1';
  FCaption := 'Form1';
  inherited Color := clBtnFace;
  inherited Caption := '';
  FEvents.Values['onClose'] := '';
  FEvents.Values['onMinimize'] := '';
  FEvents.Values['onRestore'] := '';
  FEvents.Values['onMaximize'] := '';
  FEvents.Values['onMouseMove'] := '';
  FEvents.Values['onLMouseDown'] := '';
  FEvents.Values['onLMouseUp'] := '';
  FEvents.Values['onRMouseDown'] := '';
  FEvents.Values['onRMouseUp'] := '';
  FEvents.Values['onResize'] := '';
  FEvents.Values['onDrop'] := '';
  FVisible := True;
  FEnabled := True;
  FMyCursor := craARROW;
  FStyle := $94CA0000;
  FStyleEx := $100;
  FIconID := -1;
end;

destructor Tau3Form.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure Tau3Form.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is Tau3Form) then
  begin
    (c as Tau3Form).Style := Style;
    (c as Tau3Form).Events.Assign(FEvents);
  end;
  // TODO
end;

function Tau3Form.Getau3String(FormName: string): string;
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Add('Opt("GUIOnEventMode", 1)');
    sl.Add(Format('Opt("GUIResizeMode", %d)', [FResize]));
    sl.Add(Format('$%s = GUICreate("%s", %d, %d, %d, %d, %d, %d)',
      [Name, FCaption, Width + 16, Height + 32, FLeft, FTop, FStyle, FStyleEx]));
    if FileExists(FIcon) then
      sl.Add(Format('GUISetIcon("%s", %d, $%s)', [FIcon, FIconID, Name]));
    sl.Add(Format('GUISetCursor(%d, 0, $%s)', [Ord(FMyCursor), Name]));
    sl.Add(Format('GUISetFont(%s)', [GetFontString(Font)]));
    sl.Add(Format('GUISetBkColor(0x%s, $%s)',
      [IntToHex(ColorToRGB(Color) and $00FFFFFF, 6), Name]));
    if not FEnabled then
      sl.Add('GUISetState(@SW_DISABLE)')
    else if not Visible then
      sl.Add('GUISetState(@SW_HIDE)')
    else
      sl.Add('GUISetState(@SW_SHOW)');

    sl.Add(Format('GUISetOnEvent(%d, "%sClose_Exit", $%s)',
      [GUI_EVENT_CLOSE, Name, Name]));

    if FIsMainForm then
      sl.Add('Global $PerformClose=True');
    sl.Add(Format('Func %sClose_Exit()', [Name]));
    if FEvents.Values['onClose'] <> '' then
      sl.Add('  ' + FEvents.Values['onClose'] + '()');
    if FIsMainForm then
      sl.Add('  If ($PerformClose = True) Then Exit');
    sl.Add('EndFunc');

    if (FEvents.Values['onMinimize'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onMinimize'], Name]));
    if (FEvents.Values['onRestore'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onRestore'], Name]));
    if (FEvents.Values['onMaximize'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onMaximize'], Name]));
    if (FEvents.Values['onMouseMove'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onMouseMove'], Name]));
    if (FEvents.Values['onLMouseDown'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onLMouseDown'], Name]));
    if (FEvents.Values['onLMouseUp'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onLMouseUp'], Name]));
    if (FEvents.Values['onRMouseDown'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onRMouseDown'], Name]));
    if (FEvents.Values['onRMouseUp'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onRMouseUp'], Name]));
    if (FEvents.Values['onResize'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onResize'], Name]));
    if (FEvents.Values['onDrop'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MINIMIZE, FEvents.Values['onDrop'], Name]));

    for i := 0 to Self.ControlCount - 1 do
      sl.Add((Self.Controls[i] as Iau3Component).Getau3String(FormName));
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure Tau3Form.SetText(val: string);
begin
  FCaption := val;
  Invalidate;
  if Assigned(FOnChangeCaption) then
    FOnChangeCaption(Self);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', val);
end;

procedure Tau3Form.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  g.Clear;
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

{ Edit }

function Tau3Edit.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (prop = 'styleex') or (prop = 'editstyle') or
    (Pos('ws_', prop) = 1) or (Pos('es_', prop) = 1) or (prop = 'color') or
    (prop = 'cursoricon') or (prop = 'enabled') or (prop = 'font') or
    (prop = 'hint') or (prop = 'maxlength') or (prop = 'resizing') or
    (prop = 'taborder') or (prop = 'visible');
end;

function Tau3Edit.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3Edit.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure Tau3Edit.SetLeft(Val: integer);
begin
  inherited Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure Tau3Edit.SetTop(Val: integer);
begin
  inherited Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure Tau3Edit.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure Tau3Edit.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function Tau3Edit.GetLeft: integer;
begin
  Result := inherited Left;
end;

function Tau3Edit.GetTop: integer;
begin
  Result := inherited Top;
end;

function Tau3Edit.GetWidth: integer;
begin
  Result := inherited Width;
end;

function Tau3Edit.GetHeight: integer;
begin
  Result := inherited Height;
end;

function Tau3Edit.GetText: string;
begin
  Result := inherited Text;
end;

procedure Tau3Edit.SetText(val: string);
begin
  inherited Text := val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', Val);
end;

procedure Tau3Edit.SetStyle(val: TWindowStyles);
begin
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)));
end;

procedure Tau3Edit.SetEditStyle(val: TEditStyles);
begin
  FStyle := (FStyle and (not $FFFF)) or DWord(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'EditStyle', IntToStr(cardinal(val)));
end;

procedure Tau3Edit.SetStyleEx(val: TWindowExStyles);
begin
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)));
end;

procedure Tau3Edit.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Color', IntToStr(ColorToRGB(Value) and $FFFFFF));
end;

procedure Tau3Edit.SetCursorIcon(c: TAU3Cursor);
begin
  FCursorIcon := c;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Cursor', IntToStr(cardinal(c)));
end;

procedure Tau3Edit.SetisEnabled(b: boolean);
begin
  FisEnabled := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Enabled', BoolToStr(b, True));
end;

procedure Tau3Edit.SetFont(f: TFont);
begin
  inherited Font := f;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Font', GetFontString(f));
end;

procedure Tau3Edit.SetMaxLen(l: integer);
begin
  inherited MaxLength := l;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'MaxLength', IntToStr(l));
end;

procedure Tau3Edit.SetHint(const Value: TTranslateString);
begin
  inherited SetHint(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Hint', Value);
end;

procedure Tau3Edit.SetTabOrder(i: integer);
begin
  FTabOrder := i;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Taborder', IntToStr(i));
end;

procedure Tau3Edit.SetisVisible(b: boolean);
begin
  FIsVisible := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Visible', BoolToStr(b, True));
end;

procedure Tau3Edit.SetResizing(b: TResizeModes);
begin
  FResizing := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Resizing', IntToStr(cardinal(b)));
end;

function Tau3Edit.GetFont: TFont;
begin
  Result := inherited Font;
end;

function Tau3Edit.GetMaxLen: integer;
begin
  Result := inherited MaxLength;
end;

function Tau3Edit.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function Tau3Edit.GetStyleEx: TWindowExStyles;
begin
  Result := TWindowExStyles(FStyleEX);
end;

function Tau3Edit.GetEditStyle: TEditStyles;
begin
  Result := TEditStyles(FStyle and $FFFF);
end;

function Tau3Edit.GetProp(prop: string): string;
begin
  prop := LowerCase(prop);
  if prop = 'name' then
    Result := Name
  else if prop = 'text' then
    Result := Text
  else if prop = 'x' then
    Result := IntToStr(Left)
  else if prop = 'y' then
    Result := IntToStr(Top)
  else if prop = 'width' then
    Result := IntToStr(Width)
  else if prop = 'height' then
    Result := IntToStr(Height)
  else if prop = 'style' then
    Result := IntToStr(cardinal(GetStyle))
  else if prop = 'editstyle' then
    Result := IntToStr(cardinal(GetEditStyle))
  else if prop = 'styleex' then
    Result := IntToStr(FStyleEX)
  else if prop = 'color' then
    Result := IntToStr(ColorToRGB(Color))
  else if prop = 'cursoricon' then
    Result := IntToStr(cardinal(FCursorIcon))
  else if prop = 'enabled' then
    Result := BoolToStr(FisEnabled, True)
  else if prop = 'font' then
    Result := GetFontString(Font)
  else if prop = 'hint' then
    Result := Hint
  else if prop = 'maxlength' then
    Result := IntToStr(MaxLength)
  else if prop = 'resizing' then
    Result := IntToStr(cardinal(FResizing))
  else if prop = 'taborder' then
    Result := IntToStr(FTabOrder)
  else if prop = 'Visible' then
    Result := BoolToStr(FIsVisible, True);
end;

procedure Tau3Edit.SetProp(prop, val: string);
begin
  prop := LowerCase(prop);
  if (prop = 'name') and isValidName(val) then
    Name := val
  else if prop = 'text' then
    Text := val
  else if (prop = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (prop = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (prop = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (prop = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (prop = 'style') and isNumeric(val) then
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (prop = 'editstyle') and isNumeric(val) then
    SetEditStyle(TEditStyles(StrToInt(val)))
  else if (prop = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val)
  else if (prop = 'color') and isNumeric(val) then
    inherited Color := TColor(StrToInt(val))
  else if (prop = 'cursoricon') and isNumeric(val) then
    FCursorIcon := TAU3Cursor(StrToInt(val))
  else if (prop = 'enabled') then
    FEnabled := val = 'True'
  else if (prop = 'font') then
    SetFontString(Font, val)
  else if (prop = 'hint') then
    Hint := (val)
  else if (prop = 'maxlength') and isNumeric(val) then
    MaxLength := StrToInt(val)
  else if (prop = 'resizing') and isNumeric(val) then
    FResizing := TResizeModes(StrToInt(val))
  else if (prop = 'taborder') and isNumeric(val) then
    FTabOrder := StrToInt(val)
  else if (prop = 'visible') then
    FIsVisible := val = 'True';
end;

function Tau3Edit.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

function Tau3Edit.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure Tau3Edit.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
end;

procedure Tau3Edit.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

constructor Tau3Edit.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
  FStyle := $503310C4;
  Style := [WS_BORDER];
  FCursorIcon := craARROW;
  Color := clWhite;
  FIsVisible := True;
  FisEnabled := True;
end;

destructor Tau3Edit.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure Tau3Edit.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if (c is Tau3Edit) then
  begin
    if Name = Text then
      (c as Tau3Edit).Text := c.Name
    else
      (c as Tau3Edit).Text := Text;
    (c as Tau3Edit).Style := Style;
    (c as Tau3Edit).EditStyle := EditStyle;
    (c as Tau3Edit).StyleEX := StyleEX;
    (c as Tau3Edit).Events.Assign(FEvents);
  end;
end;

function Tau3Edit.Getau3String(FormName: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add(Format('$%s = GUICtrlCreateInput("%s", %d, %d, %d, %d, %d, %d)',
      [Name, Text, Left, Top, Width, Height, FStyle, FStyleEX]));
    // MaxLength
    sl.Add(Format('GUICtrlSetLimit($%s, %d)', [Name, MaxLength]));
    // Font
    sl.Add(Format('GUICtrlSetFont($%s, %s)', [Name, GetFontString(Font)]));
    sl.Add(Format('GUICtrlSetColor($%s, 0x%s)',
      [Name, IntToHex(ColorToRGB(Font.Color) and $00FFFFFF, 6)]));
    // Background Color
    sl.Add(Format('GUICtrlSetBkColor($%s, 0x%s)',
      [Name, IntToHex(ColorToRGB(Color) and $00FFFFFF, 6)]));
    // Resizing
    sl.Add(Format('GUICtrlSetResizing($%s, %d)', [Name, cardinal(FResizing)]));
    // Visible/Enabled
    if not FIsVisible then
      sl.Add(Format('GUICtrlSetState($%s, %d)', [Name, 32]))
    else if not FisEnabled then
      sl.Add(Format('GUICtrlSetState($%s, %d)', [Name, 128]));
    // Hint
    sl.Add(Format('GUICtrlSetTip($%s, "%s")', [Name, Hint]));
    // Cursor
    sl.Add(Format('GUICtrlSetCursor($%s, %d)', [Name, cardinal(FCursorIcon)]));
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure Tau3Edit.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

{ Button }

function Tau3Button.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (prop = 'styleex') or (prop = 'buttonstyle') or
    (Pos('ws_', prop) = 1) or (Pos('bs_', prop) = 1);
end;

function Tau3Button.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3Button.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure Tau3Button.SetLeft(Val: integer);
begin
  inherited Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure Tau3Button.SetTop(Val: integer);
begin
  inherited Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure Tau3Button.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure Tau3Button.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function Tau3Button.GetLeft: integer;
begin
  Result := inherited Left;
end;

function Tau3Button.GetTop: integer;
begin
  Result := inherited Top;
end;

function Tau3Button.GetWidth: integer;
begin
  Result := inherited Width;
end;

function Tau3Button.GetHeight: integer;
begin
  Result := inherited Height;
end;

function Tau3Button.GetText: string;
begin
  Result := inherited Caption;
end;

procedure Tau3Button.SetText(val: string);
begin
  inherited Caption := val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', Val);
end;

procedure Tau3Button.SetStyle(val: TWindowStyles);
begin
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)));
end;

procedure Tau3Button.SetButtonStyle(val: TButtonStyles);
begin
  FStyle := (FStyle and (not $FFFF)) or (DWord(val));
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'ButtonStyle', IntToStr(cardinal(val)));
end;

procedure Tau3Button.SetStyleEx(val: TWindowExStyles);
begin
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)));
end;

function Tau3Button.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function Tau3Button.GetButtonStyle: TButtonStyles;
begin
  Result := TButtonStyles(FStyle and $FFFF);
end;

function Tau3Button.GetStyleEx: TWindowExStyles;
begin
  Result := TWindowExStyles(FStyleEX);
end;

function Tau3Button.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure Tau3Button.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
end;

function Tau3Button.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Caption
  else if p = 'x' then
    Result := IntToStr(Left)
  else if p = 'y' then
    Result := IntToStr(Top)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(cardinal(GetStyle))
  else if p = 'buttonstyle' then
    Result := IntToStr(cardinal(GetButtonStyle))
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX);
end;

procedure Tau3Button.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (p = 'buttonstyle') and isNumeric(val) then
    SetButtonStyle(TButtonStyles(StrToInt(val)))
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val);
end;

function Tau3Button.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure Tau3Button.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

procedure Tau3Button.Click;
var
  c: cardinal;
begin
  inherited;
  c := GetTickCount;
  if (c - FLastClick < 700) and Assigned(OnDblClick) then
    OnDblClick(Self)
  else
    FLastClick := c;
end;

constructor Tau3Button.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
end;

destructor Tau3Button.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure Tau3Button.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is Tau3Button) then
  begin
    (c as Tau3Button).CompleteStyle := CompleteStyle;
    (c as Tau3Button).StyleEX := StyleEX;
    (c as Tau3Button).Events.Assign(FEvents);
  end;
end;

function Tau3Button.Getau3String(FormName: string): string;
begin
  Result := Format('$%s = CreateButton($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Caption, Left, Top, Width, Height, FStyle, FStyleEX]);
end;

procedure Tau3Button.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

{ Checkbox }

function Tau3CheckBox.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (prop = 'styleex') or (prop = 'buttonstyle') or
    (Pos('ws_', prop) = 1) or (Pos('bs_', prop) = 1);
end;

function Tau3CheckBox.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3CheckBox.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure Tau3CheckBox.SetLeft(Val: integer);
begin
  inherited Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure Tau3CheckBox.SetTop(Val: integer);
begin
  inherited Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure Tau3CheckBox.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure Tau3CheckBox.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function Tau3CheckBox.GetLeft: integer;
begin
  Result := inherited Left;
end;

function Tau3CheckBox.GetTop: integer;
begin
  Result := inherited Top;
end;

function Tau3CheckBox.GetWidth: integer;
begin
  Result := inherited Width;
end;

function Tau3CheckBox.GetHeight: integer;
begin
  Result := inherited Height;
end;

function Tau3CheckBox.GetText: string;
begin
  Result := inherited Caption;
end;

procedure Tau3CheckBox.SetText(val: string);
begin
  inherited Caption := val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', Val);
end;

procedure Tau3Checkbox.SetStyle(val: TWindowStyles);
begin
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)));
end;

procedure Tau3Checkbox.SetButtonStyle(val: TButtonStyles);
begin
  FStyle := (FStyle and (not $FFFF)) or (DWord(val));
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'ButtonStyle', IntToStr(cardinal(val)));
end;

procedure Tau3Checkbox.SetStyleEx(val: TWindowExStyles);
begin
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)));
end;

function Tau3Checkbox.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function Tau3Checkbox.GetButtonStyle: TButtonStyles;
begin
  Result := TButtonStyles(FStyle and $FFFF);
end;

function Tau3Checkbox.GetStyleEx: TWindowExStyles;
begin
  Result := TWindowExStyles(FStyleEX);
end;

function Tau3CheckBox.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure Tau3CheckBox.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
end;

function Tau3Checkbox.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Caption
  else if p = 'x' then
    Result := IntToStr(Left)
  else if p = 'y' then
    Result := IntToStr(Top)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(cardinal(GetStyle))
  else if p = 'buttonstyle' then
    Result := IntToStr(cardinal(GetButtonStyle))
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX);
end;

procedure Tau3Checkbox.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (p = 'buttonstyle') and isNumeric(val) then
    SetButtonStyle(TButtonStyles(StrToInt(val)))
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val);
end;

function Tau3Checkbox.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure Tau3Checkbox.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

procedure Tau3Checkbox.Click;
begin
  Checked := False;
  inherited;
end;

constructor Tau3Checkbox.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
end;

destructor Tau3Checkbox.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure Tau3Checkbox.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is Tau3Checkbox) then
  begin
    (c as Tau3Checkbox).CompleteStyle := CompleteStyle;
    (c as Tau3Checkbox).StyleEX := StyleEX;
    (c as Tau3Checkbox).Events.Assign(FEvents);
  end;
end;

function Tau3Checkbox.Getau3String(FormName: string): string;
begin
  Result := Format('$%s = CreateCheckbox($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Caption, Left, Top, Width, Height, FStyle, FStyleEX]);
end;

procedure Tau3Checkbox.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

{ Label }

function Tau3Label.CheckProperty(prop: string): boolean;
begin
  prop := LowerCase(prop);
  Result := (prop = 'name') or (prop = 'text') or (prop = 'x') or
    (prop = 'y') or (prop = 'width') or (prop = 'height') or
    (prop = 'style') or (prop = 'styleex') or (prop = 'staticstyle') or
    (Pos('ws_', prop) = 1) or (Pos('ss_', prop) = 1);
end;

function Tau3Label.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3Label.SetName(const Value: TComponentName);
begin
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value);
end;

procedure Tau3Label.SetLeft(Val: integer);
begin
  inherited Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Left', IntToStr(Val));
end;

procedure Tau3Label.SetTop(Val: integer);
begin
  inherited Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Top', IntToStr(Val));
end;

procedure Tau3Label.SetWidth(Val: integer);
begin
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val));
end;

procedure Tau3Label.SetHeight(Val: integer);
begin
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val));
end;

function Tau3Label.GetLeft: integer;
begin
  Result := inherited Left;
end;

function Tau3Label.GetTop: integer;
begin
  Result := inherited Top;
end;

function Tau3Label.GetWidth: integer;
begin
  Result := inherited Width;
end;

function Tau3Label.GetHeight: integer;
begin
  Result := inherited Height;
end;

procedure Tau3Label.SetStyle(val: TWindowStyles);
begin
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)));
end;

procedure Tau3Label.SetStaticStyle(val: TStaticStyles);
begin
  FStyle := (FStyle and (not $FFFF)) or (DWord(val));
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StaticStyle', IntToStr(cardinal(val)));
end;

procedure Tau3Label.SetStyleEx(val: TWindowExStyles);
begin
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)));
end;

function Tau3Label.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function Tau3Label.GetStaticStyle: TStaticStyles;
begin
  Result := TStaticStyles(FStyle and $FFFF);
end;

function Tau3Label.GetStyleEx: TWindowExStyles;
begin
  Result := TWindowExStyles(FStyleEX);
end;

function Tau3Label.GetOnChangeProp: TPropertyChangeEvent;
begin
  Result := FOnChangeProp;
end;

procedure Tau3Label.SetOnChangeProp(a: TPropertyChangeEvent);
begin
  FOnChangeProp := a;
end;

function Tau3Label.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Caption
  else if p = 'x' then
    Result := IntToStr(Left)
  else if p = 'y' then
    Result := IntToStr(Top)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(cardinal(GetStyle))
  else if p = 'staticstyle' then
    Result := IntToStr(cardinal(GetStaticStyle))
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX);
end;

procedure Tau3Label.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    Left := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Top := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (p = 'staticstyle') and isNumeric(val) then
    SetStaticStyle(TStaticStyles(StrToInt(val)))
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val);
end;

function Tau3Label.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure Tau3Label.SetEvent(e, val: string);
begin
  FEvents.Values[e] := val;
end;

procedure Tau3Label.Paint;
var
  p, i: integer;
  sl: TStringList;
begin
  p := 0;
  sl := TStringList.Create;
  try
    sl.Text := Caption;
    for i := 0 to sl.Count - 1 do
    begin
      Canvas.TextOut(0, p, sl[i]);
      Inc(p, Canvas.TextHeight(sl[i]));
    end;
  finally
    sl.Free;
  end;
  inherited;
end;

constructor Tau3Label.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
end;

destructor Tau3Label.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure Tau3Label.CopyTo(c: TControl);
begin
  c.Left := Left;
  c.Top := Top;
  c.Width := Width;
  c.Height := Height;
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is Tau3Label) then
  begin
    (c as Tau3Label).CompleteStyle := CompleteStyle;
    (c as Tau3Label).StyleEX := StyleEX;
    (c as Tau3Label).Events.Assign(FEvents);
  end;
end;

function Tau3Label.Getau3String(FormName: string): string;
begin
  Result := Format('$%s = CreateLabel($%s, "%s", %d, %d, %d, %d, %d, %d)',
    [Name, FormName, Caption, Left, Top, Width, Height, FStyle, FStyleEX]);
end;

procedure Tau3Label.SetText(val: string);
begin
  Width := Canvas.TextWidth(val);
  Height := Canvas.TextHeight(val);
  FCaption := val;
  Invalidate;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', val);
end;

procedure Tau3Label.FillEvents(g: TValueListEditor);
var
  i: integer;
begin
  for i := 0 to FEvents.Count - 1 do
  begin
    g.Values[FEvents.Names[i]] := FEvents.ValueFromIndex[i];
    g.ItemProps[FEvents.Names[i]].EditStyle := esPickList;
  end;
end;

end.
