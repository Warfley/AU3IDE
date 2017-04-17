unit FormEditComponents;

{ TODO : OnPropChanged for Events }

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, Buttons, ValEdit,
  LCLIntf, LCLType, LCLProc, strutils, au3Types;

type
  TEditorComponent = class;
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

  IHotKeyComponent = interface
    ['{B757A944-5526-4336-8F66-314AE6E2046C}']
    function GetHotkey: string;
  end;

  IEditorComponent = interface
    ['{EC667588-9E2D-40FC-9EB8-341D1C941ED5}']
    function GetEditor: TEditorComponent;
    procedure PaintToEditor(C: TCanvas);
  end;

  { Iau3Component }

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

  { TEditorComponent }

  TEditorComponent = class(TCustomControl)
  private
    FComponent: TComponent;
    FSelected: Boolean;
    procedure SetComponent(AValue: TComponent);
    procedure SetSelected(AValue: Boolean);
  protected
    procedure Paint; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
      KeepBase: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Component: TComponent read FComponent write SetComponent;
  published
    property Selected: Boolean read FSelected write SetSelected;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
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
    FBorderHeight, FBorderWidth: integer;
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
    function GetStyleEx: TWindowExStyles;
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
    property BorderHeight: integer read FBorderHeight write FBorderHeight;
    property BorderWidth: integer read FBorderWidth write FBorderWidth;
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
    property StyleEx: TWindowExStyles read GetStyleEx write SetStyleEX;

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

  Tau3Edit = class(TCustomEdit, Iau3Component, IEditorComponent)
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
    FY, FX: integer;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetX(Val: integer);
    procedure SetY(Val: integer);
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
    function GetWidth: integer;
    function GetHeight: integer;
    function GetText: string;
  public
    function GetEditor: TEditorComponent;
    procedure PaintToEditor(C: TCanvas);
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
    property Y: integer read FY write SetY;
    property X: integer read FX write SetX;
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

  Tau3Button = class(TCustomBitBtn, Iau3Component, IHotKeyComponent, IEditorComponent)
  private
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FLastClick: cardinal;
    FOnChangeProp: TPropertyChangeEvent;
    FCursorIcon: TAU3Cursor;
    FisEnabled: boolean;
    FTabOrder: integer;
    FIsVisible: boolean;
    FResizing: TResizeModes;
    FPicture: TFilename;
    FHotKey: TShortCut;
    FX, FY: integer;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetX(Val: integer);
    procedure SetY(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetButtonStyle(val: TButtonStyles);
    procedure SetStyleEx(val: TWindowExStyles);

    procedure SetColor(Value: TColor); override;
    procedure SetCursorIcon(c: TAU3Cursor);
    procedure SetisEnabled(b: boolean);
    procedure SetFont(f: TFont);
    procedure SetHint(const Value: TTranslateString); override;
    procedure SetHotKey(h: TShortCut);
    procedure SetPicture(p: TFilename);
    procedure SetTabOrder(i: integer);
    procedure SetisVisible(b: boolean);
    procedure SetResizing(b: TResizeModes);

    function GetFont: TFont;

    function GetStyle: TWindowStyles;
    function GetButtonStyle: TButtonStyles;
    function GetStyleEx: TWindowExStyles;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetText: string;
  public
    function GetEditor: TEditorComponent;
    procedure PaintToEditor(C: TCanvas);
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
    function GetHotkey: string;
    procedure FillEvents(g: TValueListEditor);

    property Event[s: string]: string read GetEvent write SetEvent;
    property Events: TStringList read FEvents;
    property ComponentProp[prop: string]: string read GetProp write SetProp;
    property isProperty[prop: string]: boolean read CheckProperty;
  published
    property Name;
    property Y: integer read FY write SetY;
    property X: integer read FX write SetX;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Text: string read GetText write SetText;
    property Caption: string read GetText write SetText;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property ButtonStyle: TButtonStyles read GetButtonStyle write SetButtonStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property CompleteStyle: cardinal read FStyle write FStyle;
    property CursorIcon: TAU3Cursor read FCursorIcon write SetCursorIcon;
    property Enabled: boolean read FisEnabled write SetisEnabled;
    property Font: TFont read GetFont write SetFont;
    property HotKey: TShortCut read FHotKey write SetHotKey;
    property Picture: TFilename read FPicture write SetPicture;
    property Resizing: TResizeModes read FResizing write SetResizing;
    property TabOrder: integer read FTabOrder write SetTabOrder;
    property Visible: boolean read FIsVisible write SetisVisible;
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
    property TabStop;
  end;

  Tau3Checkbox = class(TCustomCheckBox, Iau3Component, IEditorComponent)
  private
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FOnChangeProp: TPropertyChangeEvent;
    FCursorIcon: TAU3Cursor;
    FisEnabled: boolean;
    FTabOrder: integer;
    FIsVisible: boolean;
    FResizing: TResizeModes;
    FX, FY: integer;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetX(Val: integer);
    procedure SetY(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetButtonStyle(val: TButtonStyles);
    procedure SetStyleEx(val: TWindowExStyles);
    procedure SetChecked(Value: Boolean); override;

    procedure SetColor(Value: TColor); override;
    procedure SetCursorIcon(c: TAU3Cursor);
    procedure SetisEnabled(b: boolean);
    procedure SetFont(f: TFont);
    procedure SetHint(const Value: TTranslateString); override;
    procedure SetTabOrder(i: integer);
    procedure SetisVisible(b: boolean);
    procedure SetResizing(b: TResizeModes);
    function GetFont: TFont;

    function GetStyle: TWindowStyles;
    function GetButtonStyle: TButtonStyles;
    function GetStyleEx: TWindowExStyles;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetText: string;
  public
    function GetEditor: TEditorComponent;
    procedure PaintToEditor(C: TCanvas);
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
    property Y: integer read FY write SetY;
    property X: integer read FX write SetX;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property ButtonStyle: TButtonStyles read GetButtonStyle write SetButtonStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property CompleteStyle: cardinal read FStyle write FStyle;
    property Text: string read GetText write SetText;
    property Caption: string read GetText write SetText;

    property CursorIcon: TAU3Cursor read FCursorIcon write SetCursorIcon;
    property Enabled: boolean read FisEnabled write SetisEnabled;
    property Font: TFont read GetFont write SetFont;
    property Resizing: TResizeModes read FResizing write SetResizing;
    property TabOrder: integer read FTabOrder write SetTabOrder;
    property Visible: boolean read FIsVisible write SetisVisible;

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
    property TabStop default True;
  end;

  Tau3Label = class(TCustomControl, Iau3Component, IEditorComponent)
  private
    FStyle: cardinal;
    FStyleEX: cardinal;
    FEvents: TStringList;
    FOnChangeProp: TPropertyChangeEvent;
    FCaption: string;
    FCursorIcon: TAU3Cursor;
    FisEnabled: boolean;
    FTabOrder: integer;
    FIsVisible: boolean;
    FResizing: TResizeModes;
    FX, FY: integer;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetX(Val: integer);
    procedure SetY(Val: integer);
    procedure SetWidth(Val: integer);
    procedure SetHeight(Val: integer);
    procedure SetText(val: string);
    procedure SetStyle(val: TWindowStyles);
    procedure SetStaticStyle(val: TStaticStyles);
    procedure SetStyleEx(val: TWindowExStyles);

    procedure SetColor(Value: TColor); override;
    procedure SetCursorIcon(c: TAU3Cursor);
    procedure SetisEnabled(b: boolean);
    procedure SetFont(f: TFont);
    procedure SetHint(const Value: TTranslateString); override;
    procedure SetTabOrder(i: integer);
    procedure SetisVisible(b: boolean);
    procedure SetResizing(b: TResizeModes);

    function GetFont: TFont;

    function GetStyle: TWindowStyles;
    function GetStaticStyle: TStaticStyles;
    function GetStyleEx: TWindowExStyles;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure Paint; override;
  public
    function GetEditor: TEditorComponent;
    procedure PaintToEditor(C: TCanvas);
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
    property Y: integer read FY write SetY;
    property X: integer read FX write SetX;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OnChangeProp: TPropertyChangeEvent read FOnChangeProp write FOnChangeProp;
    property Style: TWindowStyles read GetStyle write SetStyle;
    property StaticStyle: TStaticStyles read GetStaticStyle write SetStaticStyle;
    property StyleEX: TWindowExStyles read GetStyleEx write SetStyleEx;
    property CompleteStyle: cardinal read FStyle write FStyle;
    property Text: string read FCaption write SetText;
    property Caption: string read FCaption write SetText;

    property CursorIcon: TAU3Cursor read FCursorIcon write SetCursorIcon;
    property Enabled: boolean read FisEnabled write SetisEnabled;
    property Font: TFont read GetFont write SetFont;
    property Resizing: TResizeModes read FResizing write SetResizing;
    property TabOrder: integer read FTabOrder write SetTabOrder;
    property Visible: boolean read FIsVisible write SetisVisible;

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

function GUIEventToString(e: integer): string;
function AuColToColor(s: string): TColor;
procedure SetFontString(f: TFont; s: string);
function AU3KeyToHotKey(h: string): string;
procedure SetPos(c: TControl; pos: string);
procedure SetSize(c: TControl; Size: string);

implementation

procedure SetPos(c: TControl; pos: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ':';
    sl.DelimitedText := pos;
    (c as Iau3Component).SetProp('x', sl[0]);
    (c as Iau3Component).SetProp('y', sl[1]);
  finally
    sl.Free;
  end;
end;


procedure SetSize(c: TControl; Size: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ':';
    sl.DelimitedText := Size;
    (c as Iau3Component).SetProp('Width', sl[0]);
    (c as Iau3Component).SetProp('Height', sl[1]);
  finally
    sl.Free;
  end;
end;

function GUIEventToString(e: integer): string;
begin
  case e of
    GUI_EVENT_CLOSE: Result := 'onClose';
    GUI_EVENT_MINIMIZE: Result := 'onMinimize';
    GUI_EVENT_RESTORE: Result := 'onRestore';
    GUI_EVENT_MAXIMIZE: Result := 'onMaximize';
    GUI_EVENT_MOUSEMOVE: Result := 'onMouseMove';
    GUI_EVENT_PRIMARYDOWN: Result := 'onLMouseDown';
    GUI_EVENT_PRIMARYUP: Result := 'onLMouseUp';
    GUI_EVENT_SECONDARYUP: Result := 'onRMouseUp';
    GUI_EVENT_SECONDARYDOWN: Result := 'onRMouseDown';
    GUI_EVENT_RESIZED: Result := 'onResize';
    GUI_EVENT_DROPPED: Result := 'onDrop';
  end;
end;

function AuColToColor(s: string): TColor;
type
  TRGBCol = record
    case boolean of
      True: (R, G, B: byte);
      False: (Col: TColor);
  end;
var
  col: TRGBCol;
begin
  col.Col := 0;
  if pos('0x', s) = 1 then
    Delete(s, 1, 2);
  if Length(s) <> 6 then
    Exit(0);
  col.R := Hex2Dec(Copy(s, 1, 2));
  col.G := Hex2Dec(Copy(s, 3, 2));
  col.B := Hex2Dec(Copy(s, 5, 2));
  Result := col.Col;
end;

function ColorToAUString(c: TColor): string;
type
  TRGBCol = record
    case boolean of
      True: (R, G, B: byte);
      False: (Col: TColor);
  end;
var
  col: TRGBCol;
begin
  col.Col := ColorToRGB(c) and $00FFFFFF;
  Result := Format('0x%s%s%s', [IntToHex(col.R, 2), IntToHex(col.G, 2),
    IntToHex(col.B, 2)]);
end;

function AU3KeyToHotKey(h: string): string;
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    for i := 1 to Length(h) do
      case h[i] of
        '!': sl.Add('Alt');
        '^': sl.Add('Ctrl');
        '+': sl.Add('Shift');
        'A'..'Z': sl.Add(h[i]);
        '{':
        begin
          sl.Add(ExtractBetween(h, '{', '}'));
          Break;
        end;
      end;
    sl.Delimiter := '+';
    Result := sl.DelimitedText;
  finally
    sl.Free;
  end;
end;

function HotKeyToAu3Key(k: TShortCut): string;
var
  sl: TStringList;
  i: integer;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.Delimiter := '+';
    sl.DelimitedText := ShortCutToText(k);
    for i := 0 to sl.Count - 1 do
      if sl[i] = 'Shift' then
        Result += '+'
      else if sl[i] = 'Ctrl' then
        Result += '^'
      else if sl[i] = 'Alt' then
        Result += '!'
      else if Length(sl[i]) = 1 then
        Result += UpperCase(sl[i])
      else
        Result += Format('{%s}', [sl[i]]);
    { TODO : Control chars }
  finally
    sl.Free;
  end;
end;

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
  Result := Format('%d, %d, %d, "%s"', [f.Size, GetWeight, GetAttr, f.Name]);
end;

procedure SetFontString(f: TFont; s: string);

  function ExtractBetween(const Value, A, B: string): string;
  var
    aPos, bPos: integer;
  begin
    Result := '';
    aPos := Pos(A, Value);
    if aPos > 0 then
    begin
      aPos := aPos + Length(A);
      bPos := PosEx(B, Value, aPos);
      if bPos > 0 then
      begin
        Result := Copy(Value, aPos, bPos - aPos);
      end;
    end;
  end;

var
  sl: TStringList;
  attr: integer;
begin
  sl := TStringList.Create;
  try
    sl.CommaText := s;
    f.Name := sl[3];
    f.Size := StrToInt(Trim(sl[0]));
    f.Bold := Trim(sl[1]) = '700';
    attr := StrToInt(Trim(sl[2]));
    f.Italic := (attr and 2) = 2;
    f.Underline := (attr and 4) = 4;
    f.StrikeThrough := (attr and 8) = 8;
  finally
    sl.Free;
  end;
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

{ TEditorComponent }

procedure TEditorComponent.SetComponent(AValue: TComponent);
begin
  if FComponent = AValue then
    Exit;
  FComponent := AValue;
  if AValue is TControl then
    with AValue as TControl do
    begin
      Parent := Self;
      Left := (Width)*2;
      Self.Width := GetControlClassDefaultSize.cx;
      Self.Height := GetControlClassDefaultSize.cy;
      Visible:=True;
    end;
  Invalidate;
end;

procedure TEditorComponent.SetSelected(AValue: Boolean);
begin
  if FSelected=AValue then Exit;
  FSelected:=AValue;
  Invalidate;
end;

procedure TEditorComponent.Paint;
begin
  inherited Paint;
  Canvas.Lock;
  try
  (FComponent as IEditorComponent).PaintToEditor(Canvas);
  if Selected then
    with Canvas do
    begin
      Pen.Style := psSolid;
      Pen.Mode := pmCopy;
      Brush.Style := bsClear;
      Pen.Color := clHighlight;
      Rectangle(0, 0, ClientWidth-1, ClientHeight-1);
    end;
  finally
    Canvas.Unlock;
  end;
end;

procedure TEditorComponent.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
var
  OldLeft, OldTop, OldWidth, OldHeight: Integer;
  c: Iau3Component;
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if not (Component is TControl) then Exit;
  c:=Component as Iau3Component;
      (Component as TControl).Left := (Component as TControl).Width*2;
  OldLeft:=StrToInt(c.GetProp('X'));
  OldTop:=StrToInt(c.GetProp('Y'));
  OldWidth:=StrToInt(c.GetProp('Width'));
  OldHeight:=StrToInt(c.GetProp('Height'));
  if (ALeft<>OldLeft) or (ATop<>OldTop) then c.SetProp('pos', Format('%d:%d', [ALeft, ATop]));
  if (AWidth<>OldWidth) or (AHeight<>OldHeight) then c.SetProp('size', Format('%d:%d', [AWidth, AHeight]));
end;

constructor TEditorComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponent:=nil;
  FSelected:=False;
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
    (prop = 'font') or (prop = 'icon') or (prop = 'visible') or
    (prop = 'iconid') or (prop = 'styleex') or (prop = 'resize');
end;

function Tau3Form.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3Form.SetName(const Value: TComponentName);
var
  oldVal: string;
begin
  oldVal := Name;
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  inherited Caption := '';
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value, oldVal);
end;

procedure Tau3Form.SetLeft(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(FLeft);
  FLeft := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'x', IntToStr(Val), oldVal);
end;

procedure Tau3Form.SetTop(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(FTop);
  FTop := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'y', IntToStr(Val), oldVal);
end;

procedure Tau3Form.SetWidth(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Width);
  inherited Width := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val), oldVal);
end;

procedure Tau3Form.SetHeight(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Height);
  inherited Height := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val), oldVal);
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
var
  oldVal: string;
begin
  oldVal := BoolToStr(FEnabled, 'True', 'False');
  FEnabled := Value;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Enabled', BoolToStr(Value, 'True', 'False'), oldVal);
end;

procedure Tau3Form.SetFormCursor(Value: TAU3Cursor);
var
  oldVal: string;
begin
  oldVal := IntToStr(Ord(FMyCursor));
  FMyCursor := Value;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Cursor', IntToStr(Ord(Value)), oldVal);
end;

procedure Tau3Form.SetIcon(s: string);
var
  oldVal: string;
begin
  oldVal := FIcon;
  FIcon := s;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Icon', s, oldVal);
end;

procedure Tau3Form.SetIconID(i: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(FIconID);
  FIconID := i;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'IconID', IntToStr(i), oldVal);
end;

procedure Tau3Form.SetIsMainForm(b: boolean);
begin
  FIsMainForm := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, '', '', '');
end;

procedure Tau3Form.SetResize(val: TResizeModes);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(FResize));
  FResize := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Resize', IntToStr(cardinal(val)), oldVal);
end;

procedure Tau3Form.SetColor(Value: TColor);
var
  oldVal: string;
begin
  oldVal := IntToStr(ColorToRGB(Color) and $FFFFFF);
  inherited;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Color', IntToStr(ColorToRGB(Value) and $FFFFFF), oldVal);
end;

procedure Tau3Form.SetFont(f: TFont);
var
  oldVal: string;
begin
  oldVal := GetFontString(Font);
  inherited Font := f;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Font', GetFontString(f), oldVal);
end;

procedure Tau3Form.SetFormVisible(Value: boolean);
var
  oldVal: string;
begin
  oldVal := BoolToStr(FVisible, 'True', 'False');
  FVisible := Value;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Visible', BoolToStr(Value, 'True', 'False'), oldVal);
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
var
  oldVal: string;
begin
  oldVal := IntToStr(FStyle);
  FStyle := DWord(val) shl 16;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(FStyle), oldVal);
end;

procedure Tau3Form.SetStyleEX(val: TWindowExStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(FStyleEx);
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)), oldVal);
end;

function Tau3Form.GetStyle: TWindowStyles;
begin
  Result := TWindowStyles(FStyle shr 16);
end;

function Tau3Form.GetStyleEx: TWindowExStyles;
begin
  Result := TWindowExStyles(FStyleEx);
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
    Result := IntToStr(FResize)
  else if p = 'pos' then
    Result := Format('%d:%d', [Left, Top])
  else if p = 'size' then
    Result := Format('%d:%d', [Width, Height]);
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
    FVisible := val = 'True'
  else if (p = 'pos') then
    SetPos(Self, val)
  else if (p = 'size') then
    SetSize(Self, val);
end;

function Tau3Form.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure Tau3Form.SetEvent(e, val: string);
begin
  if Pos(e, FEvents.Text) = 0 then exit;
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
  { TODO : Extend CopyTo }
end;

function Tau3Form.Getau3String(FormName: string): string;
var
  sl, hkey: TStringList;
  i, k: integer;
  s: string;
  c: TComponent;
begin
  sl := TStringList.Create;
  try
    sl.Add('Opt("GUIOnEventMode", 1)');
    sl.Add(Format('Opt("GUIResizeMode", %d)', [FResize]));
    sl.Add(Format('$%s = GUICreate("%s", %d, %d, %d, %d, %d, %d)',
      [Name, FCaption, Width + FBorderWidth, Height + FBorderHeight,
      FLeft, FTop, FStyle, FStyleEx]));
    if FileExists(FIcon) then
      sl.Add(Format('GUISetIcon("%s", %d, $%s)', [FIcon, FIconID, Name]));
    sl.Add(Format('GUISetCursor(%d, 0, $%s)', [Ord(FMyCursor), Name]));
    sl.Add(Format('GUISetFont(%s)', [GetFontString(Font)]));
    sl.Add(Format('GUISetBkColor(%s, $%s)', [ColorToAUString(Color), Name]));
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
        [GUI_EVENT_RESTORE, FEvents.Values['onRestore'], Name]));
    if (FEvents.Values['onMaximize'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MAXIMIZE, FEvents.Values['onMaximize'], Name]));
    if (FEvents.Values['onMouseMove'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_MOUSEMOVE, FEvents.Values['onMouseMove'], Name]));
    if (FEvents.Values['onLMouseDown'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_PRIMARYDOWN, FEvents.Values['onLMouseDown'], Name]));
    if (FEvents.Values['onLMouseUp'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_PRIMARYUP, FEvents.Values['onLMouseUp'], Name]));
    if (FEvents.Values['onRMouseDown'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_SECONDARYDOWN, FEvents.Values['onRMouseDown'], Name]));
    if (FEvents.Values['onRMouseUp'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_SECONDARYUP, FEvents.Values['onRMouseUp'], Name]));
    if (FEvents.Values['onResize'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_RESIZED, FEvents.Values['onResize'], Name]));
    if (FEvents.Values['onDrop'] <> '') then
      sl.Add(Format('GUISetOnEvent(%d, "%s", $%s)',
        [GUI_EVENT_DROPPED, FEvents.Values['onDrop'], Name]));

    hkey := TStringList.Create;
    try
      for i := 0 to Self.ControlCount - 1 do
      begin

        if (Self.Controls[i] is TEditorComponent) then
          c:=(Self.Controls[i] as TEditorComponent).Component
        else
          c:=(Self.Controls[i] as TComponent);

        sl.Add((c as Iau3Component).Getau3String(FormName));
        if c is IHotKeyComponent then
        begin
          s := (c as IHotKeyComponent).GetHotkey;
          if Length(s) > 0 then
            hkey.Add(s);
        end;
      end;
      if hkey.Count > 0 then
        s := Format('Dim $%s_AccelTable[3][2] = [', [Name]);
      for k := 0 to hkey.Count - 1 do
        s += hkey[k] + ',';
      if Length(s) > 0 then
      begin
        s[Length(s)] := ']';
        sl.Add(s);
        sl.Add(Format('GUISetAccelerators($%s_AccelTable)', [Name]));
      end;
    finally
      hkey.Free;
    end;
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure Tau3Form.SetText(val: string);
var
  oldVal: string;
begin
  oldVal := FCaption;
  FCaption := val;
  Invalidate;
  if Assigned(FOnChangeCaption) then
    FOnChangeCaption(Self);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', val, oldVal);
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
    (Pos('ws_', prop) = 1) or (Pos('es_', prop) = 1) or (Pos('rz', prop) = 1) or
    (prop = 'color') or (prop = 'cursoricon') or (prop = 'enabled') or
    (prop = 'font') or (prop = 'hint') or (prop = 'maxlength') or
    (prop = 'resizing') or (prop = 'taborder') or (prop = 'visible');
end;

function Tau3Edit.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3Edit.SetName(const Value: TComponentName);
var
  oldVal: string;
begin
  oldVal := Name;
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value, oldVal);
end;

procedure Tau3Edit.SetX(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(X);
  FX := Val;
  Parent.Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'x', IntToStr(Val), oldVal);
end;

procedure Tau3Edit.SetY(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Y);
  FY := Val;
  Parent.Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'y', IntToStr(Val), oldVal);
end;

procedure Tau3Edit.SetWidth(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Width);
  inherited Width := Val;
  Parent.Width := Width;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val), oldVal);
end;

procedure Tau3Edit.SetHeight(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Height);
  inherited Height := Val;
  Parent.Height := Height;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val), oldVal);
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
var
  oldVal: string;
begin
  oldVal := Text;
  inherited Text := val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', Val, oldVal);
end;

procedure Tau3Edit.SetStyle(val: TWindowStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(GetStyle));
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)), oldVal);
end;

procedure Tau3Edit.SetEditStyle(val: TEditStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(GetEditStyle));
  FStyle := (FStyle and (not $FFFF)) or DWord(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'EditStyle', IntToStr(cardinal(val)), oldVal);
end;

procedure Tau3Edit.SetStyleEx(val: TWindowExStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(FStyleEX);
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)), oldVal);
end;

procedure Tau3Edit.SetColor(Value: TColor);
var
  oldVal: string;
begin
  oldVal := IntToStr(ColorToRGB(Color) and $FFFFFF);
  inherited SetColor(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Color', IntToStr(ColorToRGB(Value) and $FFFFFF), oldVal);
end;

procedure Tau3Edit.SetCursorIcon(c: TAU3Cursor);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(FCursorIcon));
  FCursorIcon := c;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Cursor', IntToStr(cardinal(c)), oldVal);
end;

procedure Tau3Edit.SetisEnabled(b: boolean);
var
  oldVal: string;
begin
  oldVal := BoolToStr(FisEnabled, 'True', 'False');
  FisEnabled := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Enabled', BoolToStr(b, 'True', 'False'), oldVal);
end;

procedure Tau3Edit.SetFont(f: TFont);
var
  oldVal1, oldVal2: string;
begin
  oldVal1 := GetFontString(Font);
  oldVal2 := IntToStr(ColorToRGB(Font.Color));
  inherited Font := f;
  if Assigned(FOnChangeProp) then
  begin
    FOnChangeProp(Self, 'Font', GetFontString(f), oldVal1);
    FOnChangeProp(Self, 'FontColor', IntToStr(ColorToRGB(Font.Color)), oldVal2);
  end;
end;

procedure Tau3Edit.SetMaxLen(l: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(MaxLength);
  inherited MaxLength := l;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Limit', IntToStr(l), oldVal);
end;

procedure Tau3Edit.SetHint(const Value: TTranslateString);
var
  oldVal: string;
begin
  oldVal := Hint;
  inherited SetHint(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Hint', Value, oldVal);
end;

procedure Tau3Edit.SetTabOrder(i: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(TabOrder);
  FTabOrder := i;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Taborder', IntToStr(i), oldVal);
end;

procedure Tau3Edit.SetisVisible(b: boolean);
var
  oldVal: string;
begin
  oldVal := BoolToStr(FIsVisible, 'True', 'False');
  FIsVisible := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Visible', BoolToStr(b, 'True', 'False'), oldVal);
end;

procedure Tau3Edit.SetResizing(b: TResizeModes);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(FResizing));
  FResizing := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Resizing', IntToStr(cardinal(b)), oldVal);
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
    Result := IntToStr(X)
  else if prop = 'y' then
    Result := IntToStr(Y)
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
  else if prop = 'fontcolor' then
    Result := IntToStr(ColorToRGB(Font.Color))
  else if prop = 'cursor' then
    Result := IntToStr(cardinal(FCursorIcon))
  else if prop = 'enabled' then
    Result := BoolToStr(FisEnabled, True)
  else if prop = 'font' then
    Result := GetFontString(Font)
  else if prop = 'hint' then
    Result := Hint
  else if prop = 'limit' then
    Result := IntToStr(MaxLength)
  else if prop = 'resizing' then
    Result := IntToStr(cardinal(FResizing))
  else if prop = 'taborder' then
    Result := IntToStr(FTabOrder)
  else if prop = 'Visible' then
    Result := BoolToStr(FIsVisible, True)
  else if prop = 'pos' then
    Result := Format('%d:%d', [X, Y])
  else if prop = 'size' then
    Result := Format('%d:%d', [Width, Height]);
end;

procedure Tau3Edit.SetProp(prop, val: string);
begin
  prop := LowerCase(prop);
  if (prop = 'name') and isValidName(val) then
    Name := val
  else if prop = 'text' then
    Text := val
  else if (prop = 'x') and isNumeric(val) then
    X := StrToInt(val)
  else if (prop = 'y') and isNumeric(val) then
    Y := StrToInt(val)
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
  else if (prop = 'fontcolor') and isNumeric(val) then
    Font.Color := TColor(StrToInt(val))
  else if (prop = 'cursor') and isNumeric(val) then
    FCursorIcon := TAU3Cursor(StrToInt(val))
  else if (prop = 'enabled') then
    FEnabled := val = 'True'
  else if (prop = 'font') then
    SetFontString(Font, val)
  else if (prop = 'hint') then
    Hint := (val)
  else if (prop = 'limit') and isNumeric(val) then
    MaxLength := StrToInt(val)
  else if (prop = 'resizing') and isNumeric(val) then
    FResizing := TResizeModes(StrToInt(val))
  else if (prop = 'taborder') and isNumeric(val) then
    FTabOrder := StrToInt(val)
  else if (prop = 'visible') then
    FIsVisible := val = 'True'
  else if (prop = 'pos') then
    SetPos(Self, val)
  else if (prop = 'size') then
    SetSize(Self, val);
end;

    procedure Tau3Edit.PaintToEditor(C: TCanvas);
    begin
      PaintTo(C, 0,0);
    end;

function Tau3Edit.GetEditor: TEditorComponent;
begin
  Result := Parent as TEditorComponent;
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
  FEvents.ValueFromIndex[0] := val;
end;

constructor Tau3Edit.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onChange'] := '';
  FStyle := $503310C4;
  Style := [WS_BORDER];
  FCursorIcon := craARROW;
  Color := clWhite;
  FIsVisible := True;
  FisEnabled := True;
  Self.AutoSize:=False;
end;

destructor Tau3Edit.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure Tau3Edit.CopyTo(c: TControl);
begin
  if (c is Tau3Edit) then
  begin
    if Name = Text then
      (c as Tau3Edit).Text := c.Name
    else
  (c as Tau3Edit).Width := Width;
  (c as Tau3Edit).Height := Height;
      (c as Tau3Edit).Text := Text;
  (c as Tau3Edit).Y := Y;
  (c as Tau3Edit).X := X;
    (c as Tau3Edit).Style := Style;
    (c as Tau3Edit).EditStyle := EditStyle;
    (c as Tau3Edit).StyleEX := StyleEX;
    (c as Tau3Edit).Events.Assign(FEvents);
  end;
  { TODO : Extend this shit }
end;

function Tau3Edit.Getau3String(FormName: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add(Format('$%s = GUICtrlCreateInput("%s", %d, %d, %d, %d, %d, %d)',
      [Name, Text, X, Y, Width, Height, FStyle, FStyleEX]));
    // MaxLength
    if MaxLength > 0 then
      sl.Add(Format('GUICtrlSetLimit($%s, %d)', [Name, MaxLength]));
    // Font
    sl.Add(Format('GUICtrlSetFont($%s, %s)', [Name, GetFontString(Font)]));
    if font.Color <> clBlack then
      sl.Add(Format('GUICtrlSetColor($%s, %s)', [Name, ColorToAUString(Font.Color)]));
    // Background Color
    sl.Add(Format('GUICtrlSetBkColor($%s, %s)', [Name, ColorToAUString(Color)]));
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
    if Length(FEvents.Values['onChange']) > 0 then
      sl.Add(Format('GUICtrlSetOnEvent($%s, "%s")', [Name, FEvents.Values['onChange']]));
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
    (Pos('ws_', prop) = 1) or (Pos('bs_', prop) = 1) or (Pos('rz', prop) = 1) or
    (prop = 'color') or (prop = 'cursoricon') or (prop = 'enabled') or
    (prop = 'font') or (prop = 'hint') or (prop = 'hotkey') or
    (prop = 'picture') or (prop = 'resizing') or (prop = 'taborder') or
    (prop = 'visible');
end;

function Tau3Button.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3Button.SetName(const Value: TComponentName);
var
  oldVal: string;
begin
  oldVal := Name;
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value, oldVal);
end;

procedure Tau3Button.SetX(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(X);
  FX := Val;
  Parent.Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'x', IntToStr(Val), oldVal);
end;

procedure Tau3Button.SetY(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Y);
  FY := Val;
  Parent.Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'y', IntToStr(Val), oldVal);
end;

procedure Tau3Button.SetWidth(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Width);
  inherited Width := Val;
  Parent.Width := Width;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val), oldVal);
end;

procedure Tau3Button.SetHeight(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Height);
  inherited Height := Val;
  Parent.Height := Height;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val), oldVal);
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
var
  oldVal: string;
begin
  oldVal := Caption;
  inherited Caption := val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', Val, oldVal);
end;

procedure Tau3Button.SetStyle(val: TWindowStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(GetStyle));
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)), oldVal);
end;

procedure Tau3Button.SetButtonStyle(val: TButtonStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(GetButtonStyle));
  FStyle := (FStyle and (not $FFFF)) or (DWord(val));
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'ButtonStyle', IntToStr(cardinal(val)), oldVal);
end;

procedure Tau3Button.SetStyleEx(val: TWindowExStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(FStyleEX);
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)), oldVal);
end;

procedure Tau3Button.SetColor(Value: TColor);
var
  oldVal: string;
begin
  oldVal := IntToStr(ColorToRGB(Color));
  inherited SetColor(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Color', IntToStr(ColorToRGB(Value)), oldVal);
end;

procedure Tau3Button.SetCursorIcon(c: TAU3Cursor);
var
  oldVal: string;
begin
  oldVal := IntToStr(Ord(FCursorIcon));
  FCursorIcon := c;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Cursor', IntToStr(Ord(c)), oldVal);
end;

procedure Tau3Button.SetisEnabled(b: boolean);
var
  oldVal: string;
begin
  oldVal := BoolToStr(FisEnabled, 'True', 'False');
  FisEnabled := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Enabled', BoolToStr(b, 'True', 'False'), oldVal);
end;

procedure Tau3Button.SetFont(f: TFont);
var
  oldVal1, oldVal2: string;
begin
  oldVal1 := GetFontString(Font);
  oldVal2 := IntToStr(ColorToRGB(Font.Color));
  inherited Font := f;
  if Assigned(FOnChangeProp) then
  begin
    FOnChangeProp(Self, 'Font', GetFontString(f), oldVal1);
    FOnChangeProp(Self, 'FontColor', IntToStr(ColorToRGB(Font.Color)), oldVal2);
  end;
end;

procedure Tau3Button.SetHint(const Value: TTranslateString);
var
  oldVal: string;
begin
  oldVal := Hint;
  inherited SetHint(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Hint', Value, oldVal);
end;

procedure Tau3Button.SetHotKey(h: TShortCut);
var
  oldVal: string;
begin
  oldVal := ShortCutToText(FHotKey);
  FHotKey := h;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Hotkey', ShortCutToText(h), oldVal);
end;

procedure Tau3Button.SetPicture(p: TFilename);
var
  pic: TPicture;
  ext: string;
  oldVal: string;
begin
  oldVal := FPicture;
  if not FileExists(p) then
  begin
    Glyph.Clear;
    FPicture := '';
    ButtonStyle := ButtonStyle - [BS_ICON];
  end
  else
  begin
    ext := LowerCase(ExtractFileExt(p));
    if (ext <> '.png') and (ext <> '.ico') and (ext <> '.tga') and
      (ext <> '.jpg') and (ext <> '.gif') and (ext <> '.bmp') then
    begin
      FPicture := p;
      exit;
    end;
    pic := TPicture.Create;
    try
      pic.LoadFromFile(p);
      Glyph.Assign(pic.Graphic);
    finally
      pic.Free;
    end;
    ButtonStyle := ButtonStyle + [BS_ICON];
  end;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Picture', p, oldVal);
end;

procedure Tau3Button.SetTabOrder(i: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(FTabOrder);
  FTabOrder := i;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'TabOrder', IntToStr(i), oldVal);
end;

procedure Tau3Button.SetisVisible(b: boolean);
var
  oldVal: string;
begin
  oldVal := BoolToStr(FIsVisible, 'True', 'False');
  FIsVisible := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Visible', BoolToStr(b, 'True', 'False'), oldVal);
end;

procedure Tau3Button.SetResizing(b: TResizeModes);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(FResizing));
  FResizing := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Resizing', IntToStr(cardinal(b)), oldVal);
end;

function Tau3Button.GetFont: TFont;
begin
  Result := inherited Font;
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
    Result := IntToStr(X)
  else if p = 'y' then
    Result := IntToStr(Y)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(cardinal(GetStyle))
  else if p = 'buttonstyle' then
    Result := IntToStr(cardinal(GetButtonStyle))
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX)
  else if p = 'color' then
    Result := IntToStr(ColorToRGB(Color))
  else if p = 'fontcolor' then
    Result := IntToStr(ColorToRGB(Font.Color))
  else if p = 'cursor' then
    Result := IntToStr(Ord(FCursorIcon))
  else if p = 'enabled' then
    Result := BoolToStr(FisEnabled, True)
  else if p = 'font' then
    Result := GetFontString(Font)
  else if p = 'hint' then
    Result := Hint
  else if p = 'hotkey' then
    Result := ShortCutToText(FHotKey)
  else if p = 'picture' then
    Result := FPicture
  else if p = 'resizing' then
    Result := IntToStr(cardinal(Resizing))
  else if p = 'taborder' then
    Result := IntToStr(FTabOrder)
  else if p = 'visible' then
    Result := BoolToStr(FIsVisible, True)
  else if p = 'pos' then
    Result := Format('%d:%d', [X, Y])
  else if p = 'size' then
    Result := Format('%d:%d', [Width, Height]);
end;

procedure Tau3Button.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    X := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Y := StrToInt(Val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (p = 'buttonstyle') and isNumeric(val) then
    SetButtonStyle(TButtonStyles(StrToInt(val)))
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val)
  else if (p = 'color') and isNumeric(val) then
    Color := StrToInt(val)
  else if (p = 'fontcolor') and isNumeric(val) then
    Font.Color := StrToInt(val)
  else if (p = 'cursor') and isNumeric(val) then
    FCursorIcon := TAU3Cursor(StrToInt(val))
  else if (p = 'enabled') then
    FisEnabled := val = 'True'
  else if (p = 'font') then
    SetFontString(Font, val)
  else if (p = 'hint') then
    Hint := val
  else if (p = 'hotkey') then
    FHotKey := TextToShortCut(val)
  else if (p = 'picture') then
    FPicture := val
  else if (p = 'resizing') and isNumeric(val) then
    FResizing := TResizeModes(StrToInt(val))
  else if (p = 'taborder') and isNumeric(val) then
    FTabOrder := StrToInt(val)
  else if (p = 'visible') then
    FIsVisible := val = 'True'
  else if (p = 'pos') then
    SetPos(Self, val)
  else if (p = 'size') then
    SetSize(Self, val);
end;

    procedure Tau3Button.PaintToEditor(C: TCanvas);
    begin
      PaintTo(C, 0,0);
    end;

function Tau3Button.GetEditor: TEditorComponent;
begin
  Result := Parent as TEditorComponent;
end;

function Tau3Button.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure Tau3Button.SetEvent(e, val: string);
begin
  FEvents.ValueFromIndex[0] := val;
end;

function Tau3Button.GetHotkey: string;
var
  s: string;
begin
  s := HotKeyToAu3Key(FHotKey);
  if Length(s) > 0 then
    Result := Format('["%s", $%s]', [s, Name]);
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
  FCursorIcon := craARROW;
  FIsVisible := True;
  FisEnabled := True;
end;

destructor Tau3Button.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure Tau3Button.CopyTo(c: TControl);
begin
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is Tau3Button) then
  begin
    (c as Tau3Button).Width := Width;
    (c as Tau3Button).Height := Height;
    (c as Tau3Button).X := X;
    (c as Tau3Button).Y := Y;
    (c as Tau3Button).CompleteStyle := CompleteStyle;
    (c as Tau3Button).StyleEX := StyleEX;
    (c as Tau3Button).Events.Assign(FEvents);
  end;
  { TODO : Extend }
end;

function Tau3Button.Getau3String(FormName: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('$%s = GUICtrlCreateButton("%s", %d, %d, %d, %d, %d, %d)',
      [Name, Caption, X, Y, Width, Height, FStyle, FStyleEX]);
    // Picture
    if Length(FPicture) > 0 then
      sl.Add(Format('GUICtrlSetImage($%s, "%s")', [Name, FPicture]));
    // Font
    //if Font.Name<>'default' then
    sl.Add(Format('GUICtrlSetFont($%s, %s)', [Name, GetFontString(Font)]));
    if Font.Color <> clBlack then
      sl.Add(Format('GUICtrlSetColor($%s, %s)', [Name, ColorToAUString(Font.Color)]));
    // Background Color
    if Color <> clDefault then
      sl.Add(Format('GUICtrlSetBkColor($%s, %s)', [Name, ColorToAUString(Color)]));
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
    if Length(FEvents.Values['onClick']) > 0 then
      sl.Add(Format('GUICtrlSetOnEvent($%s, "%s")', [Name, FEvents.Values['onClick']]));
    Result := sl.Text;
  finally
    sl.Free;
  end;
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
    (Pos('ws_', prop) = 1) or (Pos('bs_', prop) = 1) or (Pos('rz', prop) = 1) or
    (prop = 'color') or (prop = 'cursoricon') or (prop = 'enabled') or
    (prop = 'font') or (prop = 'hint') or (prop = 'checked') or
    (prop = 'resizing') or (prop = 'taborder') or (prop = 'visible');
end;

function Tau3CheckBox.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3CheckBox.SetName(const Value: TComponentName);
var
  oldVal: string;
begin
  oldVal := Name;
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value, oldVal);
end;

procedure Tau3CheckBox.SetX(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(X);
  FX := Val;
  Parent.Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'x', IntToStr(Val), oldVal);
end;

procedure Tau3CheckBox.SetY(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Y);
  FY := Val;
  Parent.Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'y', IntToStr(Val), oldVal);
end;

procedure Tau3CheckBox.SetWidth(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Width);
  inherited Width := Val;
  Parent.Width := Width;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val), oldVal);
end;

procedure Tau3CheckBox.SetHeight(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Height);
  inherited Height := Val;
  Parent.Height := Height;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val), oldVal);
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
var
  oldVal: string;
begin
  oldVal := Caption;
  inherited Caption := val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', Val, oldVal);
end;

procedure Tau3Checkbox.SetStyle(val: TWindowStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(GetStyle));
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)), oldVal);
end;

procedure Tau3Checkbox.SetButtonStyle(val: TButtonStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(GetButtonStyle));
  FStyle := (FStyle and (not $FFFF)) or (DWord(val));
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'ButtonStyle', IntToStr(cardinal(val)), oldVal);
end;

procedure Tau3Checkbox.SetStyleEx(val: TWindowExStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(FStyleEX);
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)), oldVal);
end;

procedure Tau3Checkbox.SetChecked(Value: Boolean);
var oldval: String;
begin
  if Value=Checked then exit;
  oldval:=BoolToStr(Checked, True);
  inherited SetChecked(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Checked', BoolToStr(Checked, True), oldval);
  GetEditor.Invalidate;
end;

procedure Tau3Checkbox.SetColor(Value: TColor);
var
  oldVal: string;
begin
  oldVal := IntToStr(ColorToRGB(Color));
  inherited SetColor(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Color', IntToStr(ColorToRGB(Value)), oldVal);
end;

procedure Tau3Checkbox.SetCursorIcon(c: TAU3Cursor);
var
  oldVal: string;
begin
  oldVal := IntToStr(Ord(FCursorIcon));
  FCursorIcon := c;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Cursor', IntToStr(Ord(c)), oldVal);
end;

procedure Tau3Checkbox.SetisEnabled(b: boolean);
var
  oldVal: string;
begin
  oldVal := BoolToStr(FisEnabled, 'True', 'False');
  FisEnabled := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Enabled', BoolToStr(b, 'True', 'False'), oldVal);
end;

procedure Tau3Checkbox.SetFont(f: TFont);
var
  oldVal1, oldVal2: string;
begin
  oldVal1 := GetFontString(Font);
  oldVal2 := IntToStr(ColorToRGB(Font.Color));
  inherited Font := f;
  if Assigned(FOnChangeProp) then
  begin
    FOnChangeProp(Self, 'Font', GetFontString(f), oldVal1);
    FOnChangeProp(Self, 'FontColor', IntToStr(ColorToRGB(Font.Color)), oldVal2);
  end;
end;

procedure Tau3Checkbox.SetHint(const Value: TTranslateString);
var
  oldVal: string;
begin
  oldVal := Hint;
  inherited SetHint(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Hint', Value, oldVal);
end;

procedure Tau3Checkbox.SetTabOrder(i: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(FTabOrder);
  FTabOrder := i;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'TabOrder', IntToStr(i), oldVal);
end;

procedure Tau3Checkbox.SetisVisible(b: boolean);
var
  oldVal: string;
begin
  oldVal := BoolToStr(FIsVisible, 'True', 'False');
  FIsVisible := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Visible', BoolToStr(b, 'True', 'False'), oldVal);
end;

procedure Tau3Checkbox.SetResizing(b: TResizeModes);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(FResizing));
  FResizing := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Resizing', IntToStr(cardinal(b)), oldVal);
end;

function Tau3Checkbox.GetFont: TFont;
begin
  Result := inherited Font;
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

procedure Tau3Checkbox.PaintToEditor(C: TCanvas);
var
  m, p, i: Integer;
  sl: TStringList;
begin
  with c do
  begin
    brush.Color:=Color;
    Brush.Style:=bsSolid;
    pen.Style:=psClear;
    FillRect(0,0, Self.Width, self.Height);
    Pen.Style:=psSolid;
    Pen.Color:=clBlack;
    Brush.Color:=clWhite;
    Brush.Style:=bsSolid;
    m := self.Height div 2;
    Rectangle(0, m - 10, 20, m + 10);
    if Checked then
    begin
    AntialiasingMode:=amOn;
      Pen.Width:=2;
      MoveTo(4,m);
      LineTo(6, m+5);
      LineTo(16, m-5);
    pen.Width:=1;
    AntialiasingMode:=amDontCare;
    end;
    Font.Assign(Self.Font);
    Brush.Style:=bsClear;

    sl := TStringList.Create;
    try
      sl.Text := Caption;
      p := m-(C.TextHeight('A')*sl.Count) div 2;
      for i := 0 to sl.Count - 1 do
      begin
        C.TextOut(26, p, sl[i]);
        Inc(p, C.TextHeight(sl[i]));
      end;
    finally
      sl.Free;
    end;
  end;
end;

function Tau3Checkbox.GetEditor: TEditorComponent;
begin
  Result := Parent as TEditorComponent;
end;

function Tau3Checkbox.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Caption
  else if p = 'x' then
    Result := IntToStr(X)
  else if p = 'y' then
    Result := IntToStr(Y)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(cardinal(GetStyle))
  else if p = 'buttonstyle' then
    Result := IntToStr(cardinal(GetButtonStyle))
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX)
  else if p = 'color' then
    Result := IntToStr(ColorToRGB(Color))
  else if p = 'fontcolor' then
    Result := IntToStr(ColorToRGB(Font.Color))
  else if p = 'cursor' then
    Result := IntToStr(Ord(FCursorIcon))
  else if p = 'enabled' then
    Result := BoolToStr(FisEnabled, True)
  else if p = 'font' then
    Result := GetFontString(Font)
  else if p = 'hint' then
    Result := Hint
  else if p = 'checked' then
    Result := BoolToStr(Checked, True)
  else if p = 'resizing' then
    Result := IntToStr(cardinal(Resizing))
  else if p = 'taborder' then
    Result := IntToStr(FTabOrder)
  else if p = 'visible' then
    Result := BoolToStr(FIsVisible, True)
  else if p = 'pos' then
    Result := Format('%d:%d', [X, Y])
  else if p = 'size' then
    Result := Format('%d:%d', [Width, Height]);
end;

procedure Tau3Checkbox.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    X := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Y := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (p = 'buttonstyle') and isNumeric(val) then
    SetButtonStyle(TButtonStyles(StrToInt(val)))
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val)
  else if (p = 'color') and isNumeric(val) then
    Color := StrToInt(val)
  else if (p = 'fontcolor') and isNumeric(val) then
    Font.Color := StrToInt(val)
  else if (p = 'cursor') and isNumeric(val) then
    FCursorIcon := TAU3Cursor(StrToInt(val))
  else if (p = 'enabled') then
    FisEnabled := val = 'True'
  else if (p = 'font') then
    SetFontString(Font, val)
  else if (p = 'hint') then
    Hint := val
  else if (p = 'checked') then
    Checked := val = 'True'
  else if (p = 'resizing') and isNumeric(val) then
    FResizing := TResizeModes(StrToInt(val))
  else if (p = 'taborder') and isNumeric(val) then
    FTabOrder := StrToInt(val)
  else if (p = 'visible') then
    FIsVisible := val = 'True'
  else if (p = 'pos') then
    SetPos(Self, val)
  else if (p = 'size') then
    SetSize(Self, val);
end;

function Tau3Checkbox.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure Tau3Checkbox.SetEvent(e, val: string);
begin
  FEvents.ValueFromIndex[0] := val;
end;

constructor Tau3Checkbox.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
  ButtonStyle := [BS_DEFPUSHBUTTON, BS_CHECKBOX];
  FIsVisible := True;
  FisEnabled := True;
end;

destructor Tau3Checkbox.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure Tau3Checkbox.CopyTo(c: TControl);
begin
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is Tau3Checkbox) then
  begin
    (c as Tau3Checkbox).Width := Width;
    (c as Tau3Checkbox).Height := Height;
    (c as Tau3Checkbox).X := X;
    (c as Tau3Checkbox).Y := Y;
    (c as Tau3Checkbox).CompleteStyle := CompleteStyle;
    (c as Tau3Checkbox).StyleEX := StyleEX;
    (c as Tau3Checkbox).Events.Assign(FEvents);
  end;
  { TODO : Extend }
end;

function Tau3Checkbox.Getau3String(FormName: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('$%s = GUICtrlCreateCheckbox("%s", %d, %d, %d, %d, %d, %d)',
      [Name, Caption, X, Y, Width, Height, FStyle, FStyleEX]);
    // Checked
    if Checked then
      sl.Add(Format('GUICtrlSetState($%s, 1)', [Name]));
    // Font
    //if Font.Name<>'default' then
    sl.Add(Format('GUICtrlSetFont($%s, %s)', [Name, GetFontString(Font)]));
    if Font.Color <> clBlack then
      sl.Add(Format('GUICtrlSetColor($%s, %s)', [Name, ColorToAUString(Font.Color)]));
    // Background Color
    if Color <> clDefault then
      sl.Add(Format('GUICtrlSetBkColor($%s, %s)', [Name, ColorToAUString(Color)]));
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
    if Length(FEvents.Values['onClick']) > 0 then
      sl.Add(Format('GUICtrlSetOnEvent($%s, "%s")', [Name, FEvents.Values['onClick']]));
    Result := sl.Text;
  finally
    sl.Free;
  end;
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
    (Pos('ws_', prop) = 1) or (Pos('ss_', prop) = 1) or (Pos('rz', prop) = 1) or
    (prop = 'color') or (prop = 'cursoricon') or (prop = 'enabled') or
    (prop = 'font') or (prop = 'hint') or (prop = 'resizing') or
    (prop = 'taborder') or (prop = 'visible');
end;

function Tau3Label.GetEvents: TStringList;
begin
  Result := FEvents;
end;

procedure Tau3Label.SetName(const Value: TComponentName);
var
  oldVal: string;
begin
  oldVal := Value;
  if Text = Name then
    Text := Value;
  inherited SetName(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Name', Value, oldVal);
end;

procedure Tau3Label.SetX(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(X);
  FX := Val;
  Parent.Left := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'x', IntToStr(Val), oldVal);
end;

procedure Tau3Label.SetY(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Y);
  FY := Val;
  Parent.Top := Val;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'y', IntToStr(Val), oldVal);
end;

procedure Tau3Label.SetWidth(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Width);
  inherited Width := Val;
  Parent.Width := Width;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Width', IntToStr(Val), oldVal);
end;

procedure Tau3Label.SetHeight(Val: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(Height);
  inherited Height := Val;
  Parent.Height := Height;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Height', IntToStr(Val), oldVal);
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
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(GetStyle));
  FStyle := (FStyle and $FFFF) or (DWord(val) shl 16);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Style', IntToStr(cardinal(val)), oldVal);
end;

procedure Tau3Label.SetStaticStyle(val: TStaticStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(GetStaticStyle));
  FStyle := (FStyle and (not $FFFF)) or (DWord(val));
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StaticStyle', IntToStr(cardinal(val)), oldVal);
end;

procedure Tau3Label.SetStyleEx(val: TWindowExStyles);
var
  oldVal: string;
begin
  oldVal := IntToStr(FStyleEX);
  FStyleEX := cardinal(val);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'StyleEx', IntToStr(cardinal(Val)), oldVal);
end;

procedure Tau3Label.SetColor(Value: TColor);
var
  oldVal: string;
begin
  oldVal := IntToStr(ColorToRGB(Color));
  inherited SetColor(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Color', IntToStr(ColorToRGB(Value)), oldVal);
end;

procedure Tau3Label.SetCursorIcon(c: TAU3Cursor);
var
  oldVal: string;
begin
  oldVal := IntToStr(Ord(FCursorIcon));
  FCursorIcon := c;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Cursor', IntToStr(Ord(c)), oldVal);
end;

procedure Tau3Label.SetisEnabled(b: boolean);
var
  oldVal: string;
begin
  oldVal := BoolToStr(FisEnabled, 'True', 'False');
  FisEnabled := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Enabled', BoolToStr(b, 'True', 'False'), oldVal);
end;

procedure Tau3Label.SetFont(f: TFont);
var
  oldVal1, oldVal2: string;
begin
  oldVal1 := GetFontString(Font);
  oldVal2 := IntToStr(ColorToRGB(Font.Color));
  inherited Font := f;
  if Assigned(FOnChangeProp) then
  begin
    FOnChangeProp(Self, 'Font', GetFontString(f), oldVal1);
    FOnChangeProp(Self, 'FontColor', IntToStr(ColorToRGB(Font.Color)), oldVal2);
  end;
end;

procedure Tau3Label.SetHint(const Value: TTranslateString);
var
  oldVal: string;
begin
  oldVal := Hint;
  inherited SetHint(Value);
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Hint', Value, oldVal);
end;

procedure Tau3Label.SetTabOrder(i: integer);
var
  oldVal: string;
begin
  oldVal := IntToStr(FTabOrder);
  FTabOrder := i;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'TabOrder', IntToStr(i), oldVal);
end;

procedure Tau3Label.SetisVisible(b: boolean);
var
  oldVal: string;
begin
  oldVal := BoolToStr(FIsVisible, 'True', 'False');
  FIsVisible := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Visible', BoolToStr(b, 'True', 'False'), oldVal);
end;

procedure Tau3Label.SetResizing(b: TResizeModes);
var
  oldVal: string;
begin
  oldVal := IntToStr(cardinal(FResizing));
  FResizing := b;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Resizing', IntToStr(cardinal(b)), oldVal);
end;

function Tau3Label.GetFont: TFont;
begin
  Result := inherited Font;
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

procedure Tau3Label.PaintToEditor(C: TCanvas);
var
  p, i: integer;
  sl: TStringList;
begin
  C.Font.Assign(Font);
  p := 0;
  C.Brush.Style := bsSolid;
  C.Brush.Color := Color;
  sl := TStringList.Create;
  try
    sl.Text := Caption;
    for i := 0 to sl.Count - 1 do
    begin
      C.TextOut(0, p, sl[i]);
      Inc(p, C.TextHeight(sl[i]));
    end;
  finally
    sl.Free;
  end;
end;

function Tau3Label.GetEditor: TEditorComponent;
begin
  Result := Parent as TEditorComponent;
end;

function Tau3Label.GetProp(p: string): string;
begin
  p := LowerCase(p);
  if p = 'name' then
    Result := Name
  else if p = 'text' then
    Result := Caption
  else if p = 'x' then
    Result := IntToStr(X)
  else if p = 'y' then
    Result := IntToStr(Y)
  else if p = 'width' then
    Result := IntToStr(Width)
  else if p = 'height' then
    Result := IntToStr(Height)
  else if p = 'style' then
    Result := IntToStr(cardinal(GetStyle))
  else if p = 'staticstyle' then
    Result := IntToStr(cardinal(GetStaticStyle))
  else if p = 'styleex' then
    Result := IntToStr(FStyleEX)
  else if p = 'color' then
    Result := IntToStr(ColorToRGB(Color))
  else if p = 'fontcolor' then
    Result := IntToStr(ColorToRGB(Font.Color))
  else if p = 'cursor' then
    Result := IntToStr(Ord(FCursorIcon))
  else if p = 'enabled' then
    Result := BoolToStr(FisEnabled, True)
  else if p = 'font' then
    Result := GetFontString(Font)
  else if p = 'hint' then
    Result := Hint
  else if p = 'resizing' then
    Result := IntToStr(cardinal(Resizing))
  else if p = 'taborder' then
    Result := IntToStr(FTabOrder)
  else if p = 'visible' then
    Result := BoolToStr(FIsVisible, True)
  else if p = 'pos' then
    Result := Format('%d:%d', [X, Y])
  else if p = 'size' then
    Result := Format('%d:%d', [Width, Height]);
end;

procedure Tau3Label.SetProp(p, val: string);
begin
  p := LowerCase(p);
  if (p = 'name') and isValidName(val) then
    Name := val
  else if p = 'text' then
    Caption := val
  else if (p = 'x') and isNumeric(val) then
    X := StrToInt(val)
  else if (p = 'y') and isNumeric(val) then
    Y := StrToInt(val)
  else if (p = 'width') and isNumeric(val) then
    Width := StrToInt(val)
  else if (p = 'height') and isNumeric(val) then
    Height := StrToInt(val)
  else if (p = 'style') and isNumeric(val) then
    SetStyle(TWindowStyles(StrToInt(val)))
  else if (p = 'staticstyle') and isNumeric(val) then
    SetStaticStyle(TStaticStyles(StrToInt(val)))
  else if (p = 'styleex') and isNumeric(val) then
    FStyleEX := StrToInt(val)
  else if (p = 'color') and isNumeric(val) then
    Color := StrToInt(val)
  else if (p = 'fontcolor') and isNumeric(val) then
    Font.Color := StrToInt(val)
  else if (p = 'cursor') and isNumeric(val) then
    FCursorIcon := TAU3Cursor(StrToInt(val))
  else if (p = 'enabled') then
    FisEnabled := val = 'True'
  else if (p = 'font') then
    SetFontString(Font, val)
  else if (p = 'hint') then
    Hint := val
  else if (p = 'resizing') and isNumeric(val) then
    FResizing := TResizeModes(StrToInt(val))
  else if (p = 'taborder') and isNumeric(val) then
    FTabOrder := StrToInt(val)
  else if (p = 'visible') then
    FIsVisible := val = 'True'
  else if (p = 'pos') then
    SetPos(Self, val)
  else if (p = 'size') then
    SetSize(Self, val);
end;

function Tau3Label.GetEvent(e: string): string;
begin
  Result := FEvents.Values[e];
end;

procedure Tau3Label.SetEvent(e, val: string);
begin
  FEvents.ValueFromIndex[0] := val;
end;

procedure Tau3Label.Paint;
begin
  PaintToEditor(Canvas);
  inherited;
end;

constructor Tau3Label.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TStringList.Create;
  FEvents.Values['onClick'] := '';
  FisEnabled := True;
  FIsVisible := True;
  FCursorIcon := craARROW;
end;

destructor Tau3Label.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure Tau3Label.CopyTo(c: TControl);
begin
  c.Width := Width;
  c.Height := Height;
  if Name = Caption then
    c.Caption := c.Name
  else
    c.Caption := Caption;
  if (c is Tau3Label) then
  begin
    (c as Tau3Label).Width := Width;
    (c as Tau3Label).Height := Height;
    (c as Tau3Label).X := X;
    (c as Tau3Label).Y := Y;
    (c as Tau3Label).CompleteStyle := CompleteStyle;
    (c as Tau3Label).StyleEX := StyleEX;
    (c as Tau3Label).Events.Assign(FEvents);
  end;
  { TODO : Extend }
end;

function Tau3Label.Getau3String(FormName: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('$%s = GUICtrlCreateLabel("%s", %d, %d, %d, %d, %d, %d)',
      [Name, Caption, x, y, Width, Height, FStyle, FStyleEX]);
    // Font
    //if Font.Name<>'default' then
    sl.Add(Format('GUICtrlSetFont($%s, %s)', [Name, GetFontString(Font)]));
    if Font.Color <> clBlack then
      sl.Add(Format('GUICtrlSetColor($%s, %s)', [Name, ColorToAUString(Font.Color)]));
    // Background Color
    if Color <> clDefault then
      sl.Add(Format('GUICtrlSetBkColor($%s, %s)', [Name, ColorToAUString(Color)]));
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
    if Length(FEvents.Values['onClick']) > 0 then
      sl.Add(Format('GUICtrlSetOnEvent($%s, "%s")', [Name, FEvents.Values['onClick']]));
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure Tau3Label.SetText(val: string);
var
  oldVal: string;
begin
  oldVal := FCaption;
  if Assigned(Parent) then
  begin
  Width := Canvas.TextWidth(val);
  Height := Canvas.TextHeight(val);
  end;
  FCaption := val;
  Invalidate;
  if Assigned(FOnChangeProp) then
    FOnChangeProp(Self, 'Text', val, oldVal);
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
