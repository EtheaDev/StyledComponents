{******************************************************************************}
{                                                                              }
{  StyledTaskDialog: a Task Dialog Component with StyleButtons                 }
{                                                                              }
{  Copyright (c) 2022-2025 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors:                                                               }
{                                                                              }
{  https://github.com/EtheaDev/StyledComponents                                }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit Vcl.StyledTaskDialog;

{$INCLUDE StyledComponents.inc}

interface

uses
  System.SysUtils
  , System.Classes
  , System.UITypes
  , WinApi.Windows
  , Vcl.Dialogs
  , Vcl.Graphics
  , Vcl.Forms
  , Vcl.ButtonStylesAttributes
  , Vcl.StandardButtonStyles
  , Vcl.StyledButton
  ;

const
  /// <summary>Default alpha blend value for styled dialogs (fully opaque)</summary>
  DEFAULT_STYLEDDIALOG_ALPHABLEND = 255;
  /// <summary>Default minimum width for styled dialogs in pixels</summary>
  DEFAULT_STYLEDDIALOG_MIN_WIDTH = 500;
  /// <summary>Default minimum height for styled dialogs in pixels</summary>
  DEFAULT_STYLEDDIALOG_MIN_HEIGHT = 280;
  /// <summary>Default width for dialog buttons in pixels</summary>
  DEFAULT_STYLEDDIALOG_BUTTONSWIDTH = 74;
  /// <summary>Default height for dialog buttons in pixels</summary>
  DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT = 30;
  /// <summary>Minimum allowed size for dialog buttons in pixels</summary>
  MIN_STYLEDDIALOG_BUTTONSSIZE = 10;
  /// <summary>Maximum allowed width for dialog buttons in pixels</summary>
  MAX_STYLEDDIALOG_BUTTONSWIDTH = 800;
  /// <summary>Maximum allowed height for dialog buttons in pixels</summary>
  MAX_STYLEDDIALOG_BUTTONSHEIGHT = 300;

  {$IFDEF Use_Large_Dialog_Icons}
  /// <summary>Default size for main dialog icon in pixels (128 when Use_Large_Dialog_Icons is defined)</summary>
  DEFAULT_MAIN_ICON_SIZE = 128;
  {$ELSE}
  /// <summary>Default size for main dialog icon in pixels</summary>
  DEFAULT_MAIN_ICON_SIZE = 64;
  {$ENDIF}

  /// <summary>Extended TTaskDialogIcon value for question mark icon</summary>
  tdiQuestion = 5;

type
  /// <summary>Array type for storing custom dialog icons indexed by dialog type</summary>
  TStyledDialogIcons = array[TMsgDlgType] of TIcon;
  /// <summary>Event type for finding a dialog button by its modal result</summary>
  /// <param name="AModalResult">The modal result to search for</param>
  /// <returns>The TStyledButton matching the modal result, or nil if not found</returns>
  TOnFindDialogButtonEvent = function (const AModalResult: TModalResult): TStyledButton of object;
  /// <summary>Exception class for styled task dialog errors</summary>
  EStyledTaskDialogException = class(Exception);

{$WARN SYMBOL_PLATFORM OFF}

type
  /// <summary>Event type called when the styled task dialog form is shown</summary>
  /// <param name="AStyledTaskDialogForm">The dialog form being shown</param>
  TTaskDialogShow = procedure(
    const AStyledTaskDialogForm: TForm) of Object;

  /// <summary>Progress bar component for styled task dialogs</summary>
  /// <remarks>Extends TTaskDialogProgressBar for use within TStyledTaskDialog</remarks>
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledTaskDialogProgressBar = class(TTaskDialogProgressBar)
  end;

  /// <summary>Advanced task dialog component with styled buttons and full customization</summary>
  /// <remarks>
  ///   TStyledTaskDialog provides a modern, fully customizable alternative to standard
  ///   Windows dialogs. It supports styled buttons, custom icons, animations (via Skia4Delphi),
  ///   progress bars, command links, radio buttons, verification checkboxes, and more.
  /// </remarks>
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledTaskDialog = class(TTaskDialog)
  private
    class var
    _DefaultButtonsFamily: TStyledButtonFamily;
    _DefaultButtonsDrawType: TStyledButtonDrawType;
    _DefaultButtonsRadius: Integer;
    _DefaultButtonsRoundedCorners: TRoundedCorners;
  private
    FHelpFile: string;
    FParentWnd: HWND;
    FPosition: TPoint;
    FMainIconSize: Integer;
    FDialogButtonsFamily: TStyledButtonFamily;
    FDialogButtonsDrawType: TStyledButtonDrawType;
    FDialogButtonsRadius: Integer;
    FDialogButtonsRoundedCorners: TRoundedCorners;
    FUseTitleInMessageDlg: Boolean;
    FAlphaBlendValue: Byte;
    FButtonsWidth: Integer;
    FButtonsHeight: Integer;
    FAutoClick: Boolean;
    FAutoClickDelay: Integer;
    FOnFindDialogButton: TOnFindDialogButtonEvent;
    FUseAnimations: Boolean;
    FUseAnimationLoop: Boolean;
    FUseAnimationInverse: Boolean;
    FUseMessageDefaultButton: Boolean;
    FMessageDefaultButton: TMsgDlgBtn;
    FOnDialogShow: TTaskDialogShow;
    FHideSystemCloseButton: Boolean;
    function IsDefaultFamily: Boolean;
    function IsStoredDrawType: Boolean;
    function IsStoredRadius: Boolean;
    function IsStoredRoundedCorners: Boolean;
    procedure SetAutoClick(const AValue: Boolean);
    procedure SetAutoClickDelay(const AValue: Integer);
    procedure SetAlphaBlendValue(const AValue: Byte);
    procedure SetButtonsHeight(const AValue: Integer);
    procedure SetButtonsWidth(const AValue: Integer);
    function GetHandle: HWND;
    procedure SetHandle(const AValue: HWND);
    procedure SetUseCommandLinks(const AValue: Boolean);
    function GetUseCommandLinks: Boolean;
    procedure SetMainIconSize(const AValue: Integer);
    procedure SetUseAnimationInverse(const AValue: Boolean);
    procedure SetUseAnimationLoop(const AValue: Boolean);
    procedure SetUseAnimations(const AValue: Boolean);
  strict protected
    function DoExecute(ParentWnd: HWND): Boolean; override;
    procedure DoOnHelp; override;
  protected
  public
    /// <summary>Called when the expand button is clicked</summary>
    /// <param name="Expanded">True if content is now expanded, False if collapsed</param>
    procedure DoOnExpandButtonClicked(Expanded: Boolean); override;
    /// <summary>Called when a radio button is clicked</summary>
    /// <param name="ButtonID">The ID of the clicked radio button</param>
    procedure DoOnRadioButtonClicked(ButtonID: Integer); override;
    /// <summary>Called when a hyperlink in the dialog text is clicked</summary>
    /// <param name="AURL">The URL of the clicked hyperlink</param>
    procedure DoOnHyperlinkClicked(const AURL: string); override;
    /// <summary>Creates a new instance of TStyledTaskDialog</summary>
    /// <param name="AOwner">The component owner</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Displays the task dialog and returns when the user closes it</summary>
    /// <param name="ParentWnd">Handle to the parent window</param>
    /// <returns>True if the dialog was closed with a positive result</returns>
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    /// <summary>Finds a dialog button by its modal result value</summary>
    /// <param name="AModalResult">The modal result to search for</param>
    /// <returns>The TStyledButton matching the modal result, or nil if not found</returns>
    function FindDialogButton(const AModalResult: TModalResult): TStyledButton;
    /// <summary>Returns True if a custom position has been set for the dialog</summary>
    /// <returns>True if Position is not the default (-1, -1)</returns>
    function IsCustomPosition: Boolean;
    /// <summary>Registers default rendering style for all TStyledTaskDialog instances</summary>
    /// <param name="ADrawType">The draw type for buttons (btRoundRect, btRounded, btRect, btEllipse)</param>
    /// <param name="AFamily">The style family for buttons (default: DEFAULT_CLASSIC_FAMILY)</param>
    /// <param name="ARadius">The corner radius in pixels (default: DEFAULT_RADIUS)</param>
    /// <param name="ARoundedCorners">Which corners to round (default: ALL_ROUNDED_CORNERS)</param>
    class procedure RegisterDefaultRenderingStyle(
      const ADrawType: TStyledButtonDrawType;
      const AFamily: TStyledButtonFamily = DEFAULT_CLASSIC_FAMILY;
      const ARadius: Integer = DEFAULT_RADIUS;
      const ARoundedCorners: TRoundedCorners = ALL_ROUNDED_CORNERS); virtual;
    /// <summary>The default button determined from message dialog type</summary>
    property MessageDefaultButton: TMsgDlgBtn read FMessageDefaultButton;
    /// <summary>Indicates if MessageDefaultButton should be used</summary>
    property UseMessageDefaultButton: Boolean read FUseMessageDefaultButton;
    /// <summary>Path to the help file for context-sensitive help</summary>
    property HelpFile: string read FHelpFile write FHelpFile;
    /// <summary>Window handle of the dialog form</summary>
    property Handle: HWND read GetHandle write SetHandle;
    /// <summary>Event handler to find a dialog button by modal result</summary>
    property OnFindDialogButton: TOnFindDialogButtonEvent read FOnFindDialogButton write FOnFindDialogButton;
  published
    /// <summary>Enables automatic button click after a delay</summary>
    property AutoClick: Boolean read FAutoClick write SetAutoClick default False;
    /// <summary>Delay in milliseconds before auto-clicking the default button</summary>
    property AutoClickDelay: Integer read FAutoClickDelay write SetAutoClickDelay default DEFAULT_AUTOCLICK_DELAY;
    /// <summary>Style family for dialog buttons (e.g., Bootstrap, Angular, Classic)</summary>
    property DialogButtonsFamily: TStyledButtonFamily read FDialogButtonsFamily write FDialogButtonsFamily stored IsDefaultFamily;
    /// <summary>Draw type for dialog buttons (btRoundRect, btRounded, btRect, btEllipse)</summary>
    property DialogButtonsDrawType: TStyledButtonDrawType read FDialogButtonsDrawType write FDialogButtonsDrawType stored IsStoredDrawType;
    /// <summary>Corner radius in pixels for dialog buttons</summary>
    property DialogButtonsRadius: Integer read FDialogButtonsRadius write FDialogButtonsRadius stored IsStoredRadius;
    /// <summary>Specifies which corners of dialog buttons should be rounded</summary>
    property DialogButtonsRoundedCorners: TRoundedCorners read FDialogButtonsRoundedCorners write FDialogButtonsRoundedCorners stored IsStoredRoundedCorners;
    /// <summary>When True, displays buttons as command links with descriptions</summary>
    property UseCommandLinks: Boolean read GetUseCommandLinks write SetUseCommandLinks default False;
    /// <summary>Enables Lottie animations for dialog icons (requires Skia4Delphi)</summary>
    property UseAnimations: Boolean read FUseAnimations write SetUseAnimations default false;
    /// <summary>When True, loops the icon animation continuously</summary>
    property UseAnimationLoop: Boolean read FUseAnimationLoop write SetUseAnimationLoop default False;
    /// <summary>When True, plays the icon animation in reverse</summary>
    property UseAnimationInverse: Boolean read FUseAnimationInverse write SetUseAnimationInverse default False;
    /// <summary>When True, displays title in simple message dialogs</summary>
    property UseTitleInMessageDlg: Boolean read FUseTitleInMessageDlg write FUseTitleInMessageDlg default True;
    /// <summary>Alpha blend value for dialog transparency (0=transparent, 255=opaque)</summary>
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue default DEFAULT_STYLEDDIALOG_ALPHABLEND;
    /// <summary>Width of dialog buttons in pixels</summary>
    property ButtonsWidth: Integer read FButtonsWidth write SetButtonsWidth default DEFAULT_STYLEDDIALOG_BUTTONSWIDTH;
    /// <summary>Height of dialog buttons in pixels</summary>
    property ButtonsHeight: Integer read FButtonsHeight write SetButtonsHeight default DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT;
    /// <summary>When True, hides the system close button (X) in the title bar</summary>
    property HideSystemCloseButton: Boolean read FHideSystemCloseButton write FHideSystemCloseButton default False;
    /// <summary>Custom position for the dialog. Use (-1, -1) for default centering</summary>
    property Position: TPoint read FPosition write FPosition stored IsCustomPosition;
    /// <summary>Size of the main dialog icon in pixels</summary>
    property MainIconSize: Integer read FMainIconSize write SetMainIconSize default DEFAULT_MAIN_ICON_SIZE;
    /// <summary>Event fired when the dialog form is shown</summary>
    property OnDialogShow: TTaskDialogShow read FOnDialogShow write FOnDialogShow;
  end;

  /// <summary>Interface for custom dialog launcher implementations</summary>
  /// <remarks>Implement this interface to provide custom dialog display logic</remarks>
  ITaskDialogLauncher = interface
    ['{B2F16F98-C163-4706-A803-E624126D8DF6}']
    /// <summary>Executes and displays the task dialog</summary>
    /// <param name="ParentWnd">Handle to the parent window</param>
    /// <param name="ADialogType">The type of dialog (mtWarning, mtError, etc.)</param>
    /// <param name="ATaskDialog">The TStyledTaskDialog instance to display</param>
    /// <param name="ADialogBtnFamily">The button style family to use</param>
    /// <returns>True if the dialog was executed successfully</returns>
    function DoExecute(ParentWnd: HWND;
      const ADialogType: TMsgDlgType;
      const ATaskDialog: TStyledTaskDialog;
      const ADialogBtnFamily: TStyledButtonFamily): boolean;
  end;


//Deprecated functions
function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload; deprecated 'use StyledTaskMessageDlg(...) instead'
function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload; deprecated 'use StyledTaskMessageDlgPos(...) instead'
function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1): Integer; overload; deprecated 'use StyledTaskMessageDlgPos(...) or StyledTaskMessageDlg(...) instead'
function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1): Integer; overload; deprecated 'use StyledTaskMessageDlgPos(...) or StyledTaskMessageDlg(...) instead'
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload; deprecated 'use StyledMessageDlgPos(...) changing position of DefaultButton param, after HelpCtx'

/// <summary>Displays a styled message dialog (replacement for MessageDlg)</summary>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom)</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <param name="DefaultButton">The default button</param>
/// <returns>The modal result of the clicked button</returns>
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
/// <summary>Displays a styled message dialog (replacement for MessageDlg)</summary>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom)</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <returns>The modal result of the clicked button</returns>
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
/// <summary>Displays a styled message dialog with custom button captions</summary>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <param name="DefaultButton">The default button</param>
/// <param name="CustomButtonCaptions">Array of custom captions for each button</param>
/// <returns>The modal result of the clicked button</returns>
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  CustomButtonCaptions: array of string): Integer; overload;

/// <summary>Displays a styled message dialog at a specific position</summary>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <param name="X">X coordinate for dialog position</param>
/// <param name="Y">Y coordinate for dialog position</param>
/// <returns>The modal result of the clicked button</returns>
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload;
/// <summary>Displays a styled message dialog at a specific position with default button</summary>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <param name="DefaultButton">The default button</param>
/// <param name="X">X coordinate for dialog position</param>
/// <param name="Y">Y coordinate for dialog position</param>
/// <returns>The modal result of the clicked button</returns>
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer; overload;

/// <summary>Displays a styled task message dialog with title (replacement for TaskMessageDlg)</summary>
/// <param name="Title">The dialog title</param>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <returns>The modal result of the clicked button</returns>
function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
/// <summary>Displays a styled task message dialog with title and default button</summary>
/// <param name="Title">The dialog title</param>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <param name="DefaultButton">The default button</param>
/// <returns>The modal result of the clicked button</returns>
function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
/// <summary>Displays a styled task message dialog at a specific position</summary>
/// <param name="Title">The dialog title</param>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <param name="X">X coordinate for dialog position</param>
/// <param name="Y">Y coordinate for dialog position</param>
/// <returns>The modal result of the clicked button</returns>
function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload;
/// <summary>Displays a styled task message dialog at a specific position with default button</summary>
/// <param name="Title">The dialog title</param>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <param name="DefaultButton">The default button</param>
/// <param name="X">X coordinate for dialog position</param>
/// <param name="Y">Y coordinate for dialog position</param>
/// <returns>The modal result of the clicked button</returns>
function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer; overload;

/// <summary>Displays a styled task message dialog with extended options</summary>
/// <param name="Instruction">The instruction/title text</param>
/// <param name="Msg">The message to display</param>
/// <param name="DlgType">The dialog type</param>
/// <param name="Buttons">The buttons to display</param>
/// <param name="HelpCtx">Help context ID</param>
/// <param name="AutoClickDelay">Delay in ms for auto-click (-1 to disable)</param>
/// <param name="UseCommandLinks">When True, displays buttons as command links</param>
/// <returns>The modal result of the clicked button</returns>
function DoStyledTaskMessageDlg(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx : Longint = 0;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False): Integer; overload;
/// <summary>Displays a styled task message dialog with position and extended options</summary>
function DoStyledTaskMessageDlgPos(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False): Integer; overload;
/// <summary>Displays a styled task message dialog with position, help file and extended options</summary>
function DoStyledTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
/// <summary>Displays a styled task message dialog with all options including default button</summary>
function DoStyledTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
/// <summary>Displays a message dialog with position and help support</summary>
function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Integer; DefaultButton: TMsgDlgBtn;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1;  const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
/// <summary>Displays a message dialog with position and help support (without default button)</summary>
function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;

/// <summary>Displays a simple styled message (replacement for ShowMessage)</summary>
/// <param name="Msg">The message to display</param>
procedure StyledShowMessage(const Msg: string); overload;
/// <summary>Displays a formatted styled message (replacement for ShowMessageFmt)</summary>
/// <param name="Msg">The format string</param>
/// <param name="Params">The format parameters</param>
procedure StyledShowMessageFmt(const Msg: string; Params: array of const); overload;

/// <summary>Sets whether to display title in message dialogs</summary>
/// <param name="AValue">True to display title, False to hide it</param>
procedure SetUseTitleInMessageDlg(AValue: boolean);
/// <summary>Registers a custom dialog launcher implementation</summary>
/// <param name="AShowStyledTaskDialog">The custom launcher interface</param>
/// <param name="AButtonFamily">The button style family to use</param>
procedure RegisterCustomExecute(const AShowStyledTaskDialog: ITaskDialogLauncher;
  const AButtonFamily: TStyledButtonFamily = '');
/// <summary>Unregisters the custom dialog launcher</summary>
procedure UnregisterCustomExecute;
/// <summary>Converts a TTaskDialogIcon to TMsgDlgType</summary>
/// <param name="AIcon">The task dialog icon</param>
/// <returns>The corresponding message dialog type</returns>
function GetTaskDlgType(const AIcon: TTaskDialogIcon): TMsgDlgType;
/// <summary>Returns the current dialog font</summary>
/// <returns>The TFont used for dialogs</returns>
function GetDialogFont: TFont;
/// <summary>Returns the current dialog button family</summary>
/// <returns>The style family name</returns>
function GetDialogBtnFamily: TStyledButtonFamily;

/// <summary>Returns the default title for a dialog type</summary>
/// <param name="DlgType">The dialog type</param>
/// <returns>The localized title string</returns>
function GetDialogTypeTitle(const DlgType: TMsgDlgType): string;
/// <summary>Returns the current dialog alpha blend value</summary>
/// <returns>The alpha blend value (0-255)</returns>
function GetDialogAlphaBlendValue: Byte;
/// <summary>Converts a TMsgDlgBtn to TTaskDialogCommonButton</summary>
/// <param name="ABtn">The message dialog button</param>
/// <returns>The corresponding task dialog common button</returns>
function MsgDlgBtnToDefaultBtn(const ABtn: TMsgDlgBtn): TTaskDialogCommonButton;

/// <summary>Initializes styled task dialogs with a custom font</summary>
/// <param name="AFont">The font to use for dialogs</param>
procedure InitializeStyledTaskDialogs(AFont: TFont); overload;

/// <summary>Initializes styled task dialogs with animation setting</summary>
/// <param name="AUseAnimations">True to enable animations (requires Skia4Delphi)</param>
procedure InitializeStyledTaskDialogs(AUseAnimations: Boolean); overload;

/// <summary>Initializes styled task dialogs with button family and options</summary>
/// <param name="ADialogButtonsFamily">The button style family</param>
/// <param name="AUseCommandLinks">True to use command links style</param>
/// <param name="AAutoClickDelay">Auto-click delay in milliseconds (-1 to disable)</param>
procedure InitializeStyledTaskDialogs(
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AUseCommandLinks: Boolean = False;
  const AAutoClickDelay: Integer = -1); overload;

/// <summary>Initializes styled task dialogs with full customization options</summary>
/// <param name="AUseTitleInMessageDlg">True to show title in message dialogs</param>
/// <param name="AFont">The font to use for dialogs</param>
/// <param name="AUseCommandLinks">True to use command links style</param>
/// <param name="ADialogButtonsFamily">The button style family</param>
/// <param name="AAlphaBlendValue">Dialog transparency (0-255)</param>
/// <param name="AButtonsWidth">Width of dialog buttons in pixels</param>
/// <param name="AButtonsHeight">Height of dialog buttons in pixels</param>
/// <param name="AAutoClickDelay">Auto-click delay in milliseconds (-1 to disable)</param>
procedure InitializeStyledTaskDialogs(
  AUseTitleInMessageDlg: Boolean; AFont: TFont;
  const AUseCommandLinks: Boolean = False;
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AAlphaBlendValue: Byte = DEFAULT_STYLEDDIALOG_ALPHABLEND;
  const AButtonsWidth: Integer = DEFAULT_STYLEDDIALOG_BUTTONSWIDTH;
  const AButtonsHeight: Integer = DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT;
  const AAutoClickDelay: Integer = -1); overload;

implementation

uses
  System.TypInfo
  , System.Math
  , System.Types
  , System.Rtti
  , Vcl.Themes
  , Winapi.CommCtrl
  , System.WideStrUtils
  , Winapi.MultiMon
  , System.HelpIntfs
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Consts
  , System.RTLConsts
  , Winapi.ShellApi
  , Vcl.StyledCmpMessages
  , Vcl.StyledCmpStrUtils
  , Vcl.StyledTaskDialogFormUnit
  , Vcl.StyledTaskDialogStdUnit
  ;

var
  _TaskDialogExecute: ITaskDialogLauncher;
  _DialogButtonsFamily: TStyledButtonFamily;
  _CustomIcons: TStyledDialogIcons;
  _UseAnimations: Boolean;
  _DialogFont: TFont;
  _UseCommandLinks: Boolean;
  _UseTitleInMessageDlg: boolean;
  _AlphaBlendValue: Byte;
  _ButtonsWidth: Integer;
  _ButtonsHeight: Integer;
  _AutoClickDelay: Integer;

  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll','Help','Close');
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0, mrClose);

function GetDefaultButton(const Buttons: TMsgDlgButtons): TMsgDlgBtn;
begin
  if      mbYes       in Buttons then Result := TMsgDlgBtn.mbYes
  else if mbOK        in Buttons then Result := TMsgDlgBtn.mbOK
  else if mbNo        in Buttons then Result := TMsgDlgBtn.mbNo
  else if mbCancel    in Buttons then Result := TMsgDlgBtn.mbCancel
  else if mbAbort     in Buttons then Result := TMsgDlgBtn.mbAbort
  else if mbRetry     in Buttons then Result := TMsgDlgBtn.mbRetry
  else if mbIgnore    in Buttons then Result := TMsgDlgBtn.mbIgnore
  else if mbAll       in Buttons then Result := TMsgDlgBtn.mbAll
  else if mbNoToAll   in Buttons then Result := TMsgDlgBtn.mbNoToAll
  else if mbYesToAll  in Buttons then Result := TMsgDlgBtn.mbYesToAll
  else if mbHelp      in Buttons then Result := TMsgDlgBtn.mbHelp
  else if mbClose     in Buttons then Result := TMsgDlgBtn.mbClose
  else Result := TMsgDlgBtn.mbRetry;
end;

//Internal Function to Display a Styled TaskMessage Dialog
//using a TStyledTaskDialog component
function InternalDoTaskMessageDlgPosHelp(const Instruction, Msg: string;
  const DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  const X, Y: Integer;
  const HelpFileName: string;
  const DefaultButton: TMsgDlgBtn;
  const AutoClickDelay: Integer;
  const UseCommandLinks: Boolean;
  const UseAlternateTaskDlgFlags: Boolean;
  const AlternateTaskDlgFlags: TTaskDialogFlags;
  const CustomButtonCaptions: array of string): Integer;
const
  IconMap: array[TMsgDlgType] of TTaskDialogIcon = (tdiWarning, tdiError,
    tdiInformation, tdiQuestion, tdiShield);
  LModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, mrHelp, mrClose);
var
  DlgBtn: TMsgDlgBtn;
  LTaskDialog: TStyledTaskDialog;
  LTaskDialogButtonItem: TTaskDialogButtonItem;
  LCommandLinkPresent: Boolean;
  LIndex: Integer;
begin
  //At least OK Button
  if Buttons = [] then
    Buttons := [TMsgDlgBtn.mbOK];

  Application.ModalStarted;
  LTaskDialog := TStyledTaskDialog.Create(nil);
  try
    //Assign custom position to Dialog
    LTaskDialog.Position := TPoint.Create(X, Y);
    //Reset default CommonButtons
    LTaskDialog.CommonButtons := [];
    //To inform the Dialog Form to use the Default Button specified in MessageDlg
    LTaskDialog.FUseMessageDefaultButton := True;
    LTaskDialog.FMessageDefaultButton := DefaultButton;
    LTaskDialog.UseAnimations :=  _UseAnimations;
    LTaskDialog.AutoClick := AutoClickDelay > 0;
    if LTaskDialog.AutoClick then
      LTaskDialog.AutoClickDelay := AutoClickDelay;
    LTaskDialog.UseCommandLinks := UseCommandLinks;
    if UseAlternateTaskDlgFlags then
      LTaskDialog.Flags := AlternateTaskDlgFlags; //Replace the default Flags to AlternateTaskDlgFlags.
    // Assign buttons
    LIndex := -1;
    LCommandLinkPresent := False;
    for DlgBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      //Check for particular case using command links:
      if UseCommandLinks then
      begin
        //if mbYes and mbOK are both present with "UseCommandLinks", then tcbOk is used as CommonButton
        if (DlgBtn = TMsgDlgBtn.mbOK) and (DlgBtn in Buttons) and (TMsgDlgBtn.mbYes in Buttons) then
        begin
          LTaskDialog.CommonButtons := LTaskDialog.CommonButtons + [TTaskDialogCommonButton.tcbOk];
          Continue;
        end;
        //if mbNo and mbCancel are both present with "UseCommandLinks", then tcbCancel is used as CommonButton
        if (DlgBtn = TMsgDlgBtn.mbCancel) and (DlgBtn in Buttons) and (TMsgDlgBtn.mbNo in Buttons) then
        begin
          LTaskDialog.CommonButtons := LTaskDialog.CommonButtons + [TTaskDialogCommonButton.tcbCancel];
          Continue;
        end;
      end;

      if DlgBtn in Buttons then
      begin
        //if a Button is "common", add only if not using CommandLinks
        LTaskDialogButtonItem := LTaskDialog.Buttons.Add as TTaskDialogButtonItem;
        Inc(LIndex);
        if LIndex <= High(CustomButtonCaptions) then
          LTaskDialogButtonItem.Caption := CustomButtonCaptions[LIndex]
        else
        begin
          //use Captions defined in Vcl.StyledCmpMessages.pas
          case DlgBtn of
            mbYes: LTaskDialogButtonItem.Caption := STR_YES;
            mbNo: LTaskDialogButtonItem.Caption := STR_NO;
            mbOK: LTaskDialogButtonItem.Caption := STR_OK;
            mbCancel: LTaskDialogButtonItem.Caption := STR_CANCEL;
            mbAbort: LTaskDialogButtonItem.Caption := STR_ABORT;
            mbRetry: LTaskDialogButtonItem.Caption := STR_RETRY;
            mbIgnore: LTaskDialogButtonItem.Caption := STR_IGNORE;
            mbAll: LTaskDialogButtonItem.Caption := STR_ALL;
            mbNoToAll: LTaskDialogButtonItem.Caption := STR_NOTOALL;
            mbYesToAll: LTaskDialogButtonItem.Caption := STR_YESTOALL;
            mbHelp: LTaskDialogButtonItem.Caption := STR_HELP;
            mbClose: LTaskDialogButtonItem.Caption := STR_CLOSE;
          end;
        end;

        //Set CommandLinkHint based on DlgBtn when using a Warning or Confirmation
        if DlgType in [TMsgDlgType.mtWarning, TMsgDlgType.mtConfirmation] then
        case DlgBtn of
          mbOK: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CONFIRM, STR_THE_OPERATION]);
          mbYes: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CONFIRM, STR_THE_OPERATION]);
          mbNo: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CANCEL, STR_THE_OPERATION]);
          mbCancel: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CANCEL, STR_THE_OPERATION]);
          mbAbort: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_ABORT, STR_THE_OPERATION]);
          mbRetry: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_RETRY, STR_THE_OPERATION]);
          mbIgnore: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_IGNORE, STR_THE_OPERATION]);
          mbNoToAll: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CANCEL, STR_THE_OPERATION]);
          mbYesToAll: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CONFIRM, STR_THE_OPERATION]);
        end;

        if LTaskDialogButtonItem.CommandLinkHint <> '' then
          LCommandLinkPresent := UseCommandLinks;

        if DlgBtn = DefaultButton then
          LTaskDialogButtonItem.Default := True;
        LTaskDialogButtonItem.ModalResult := LModalResults[DlgBtn];
      end;
    end;

    //use Captions defined in Vcl.StyledCmpMessages.pas
    LTaskDialog.Caption := GetDialogTypeTitle(DlgType);
    if Not UseAlternateTaskDlgFlags and Application.UseRightToLeftReading then
      LTaskDialog.Flags := LTaskDialog.Flags + [tfRtlLayout];
    if pos('<A HREF=',Msg) > 0 then
      LTaskDialog.Flags := LTaskDialog.Flags + [tfEnableHyperlinks];

    if LTaskDialog.UseCommandLinks and LCommandLinkPresent then
      LTaskDialog.Flags := LTaskDialog.Flags + [tfUseCommandLinks];

    LTaskDialog.HelpContext := HelpCtx;
    LTaskDialog.HelpFile := HelpFileName;
    LTaskDialog.MainIcon := IconMap[DlgType];
    if (X <> -1) and (Y <> -1) then
      LTaskDialog.Position := Point(X, Y);
    LTaskDialog.Text := Msg;

    if (Instruction = '') and (LTaskDialog.UseTitleInMessageDlg) then
      LTaskDialog.Title := LTaskDialog.Caption
    else
      LTaskDialog.Title := Instruction;

    // Show dialog and return result
    Result := mrNone;
    if LTaskDialog.Execute then
      Result := LTaskDialog.ModalResult;
  finally
    LTaskDialog.Free;
    Application.ModalFinished;
  end;
end;

function DoStyledTaskMessageDlg(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint = 0;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
    Buttons, HelpCtx, -1, -1, '', GetDefaultButton(Buttons),
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoStyledTaskMessageDlgPos(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
    Buttons, HelpCtx, X, Y, '', GetDefaultButton(Buttons),
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoStyledTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1;  const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
    Buttons, HelpCtx, X, Y, HelpFileName, GetDefaultButton(Buttons),
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoStyledTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
    Buttons, HelpCtx, X, Y, HelpFileName, DefaultButton,
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Integer; DefaultButton: TMsgDlgBtn;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp('', Msg, DlgType,
    Buttons, HelpCtx, X, Y, HelpFileName, DefaultButton,
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp('', Msg, DlgType,
    Buttons, HelpCtx, X, Y, HelpFileName, GetDefaultButton(Buttons),
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function GetDialogAlphaBlendValue: Byte;
begin
  Result := _AlphaBlendValue;
end;

function GetDialogButtonsWidth: Integer;
begin
  Result := _ButtonsWidth;
end;

function GetDialogFont: TFont;
begin
  Result := _DialogFont;
end;

function GetDialogBtnFamily: TStyledButtonFamily;
begin
  Result := _DialogButtonsFamily;
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx,
    -1, -1, _AutoClickDelay, _UseCommandLinks);
end;

function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx);
end;

function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx,
    DefaultButton, X, Y);
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  CustomButtonCaptions: array of string): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx,
    -1, -1, '', DefaultButton, -1,
    False, False, [], CustomButtonCaptions);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx, X, Y);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons,
    HelpCtx, DefaultButton, X, Y, -1, _UseCommandLinks, '');
end;

procedure InitializeStyledTaskDialogs(AUseAnimations: Boolean);
begin
  _UseAnimations  := AUseAnimations;
end;

procedure InitializeStyledTaskDialogs(AFont: TFont);
begin
  if Assigned(AFont) then
  begin
    if not Assigned(_DialogFont) then
      _DialogFont := TFont.Create;
    _DialogFont.Assign(AFont);
  end
  else
    FreeAndNil(_DialogFont);
end;

procedure InitializeStyledTaskDialogs(
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AUseCommandLinks: Boolean = False;
  const AAutoClickDelay: Integer = -1); overload;
begin
  InitializeStyledTaskDialogs(True, Screen.MessageFont,
    AUseCommandLinks, ADialogButtonsFamily,
    DEFAULT_STYLEDDIALOG_ALPHABLEND,
    DEFAULT_STYLEDDIALOG_BUTTONSWIDTH,
    DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT,
    AAutoClickDelay);
end;

procedure InitializeStyledTaskDialogs(AUseTitleInMessageDlg: Boolean; AFont: TFont;
  const AUseCommandLinks: Boolean = False;
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AAlphaBlendValue: Byte = DEFAULT_STYLEDDIALOG_ALPHABLEND;
  const AButtonsWidth: Integer = DEFAULT_STYLEDDIALOG_BUTTONSWIDTH;
  const AButtonsHeight: Integer = DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT;
  const AAutoClickDelay: Integer = -1);
begin
  CheckValue('TStyledTaskDialog.ButtonsHeight', AButtonsHeight,
    MIN_STYLEDDIALOG_BUTTONSSIZE, MAX_STYLEDDIALOG_BUTTONSHEIGHT);
  CheckValue('TStyledTaskDialog.SetButtonsWidth', AButtonsWidth,
    MIN_STYLEDDIALOG_BUTTONSSIZE, MAX_STYLEDDIALOG_BUTTONSWIDTH);
  if Assigned(AFont) then
  begin
    if not Assigned(_DialogFont) then
      _DialogFont := TFont.Create;
    _DialogFont.Assign(AFont);
  end
  else
    FreeAndNil(_DialogFont);
  _UseCommandLinks := AUseCommandLinks;
  _UseTitleInMessageDlg := AUseTitleInMessageDlg;
  _DialogButtonsFamily := ADialogButtonsFamily;
  _AlphaBlendValue := AAlphaBlendValue;
  _ButtonsWidth := AButtonsWidth;
  _ButtonsHeight := AButtonsHeight;
  _AutoClickDelay := AAutoClickDelay;
end;

procedure UnregisterCustomIcons;
begin
  _CustomIcons[mtWarning] := nil;
  _CustomIcons[mtError] := nil;
  _CustomIcons[mtInformation] := nil;
  _CustomIcons[mtConfirmation] := nil;
  _CustomIcons[mtCustom] := nil;
end;

procedure RegisterCustomIcons(const ACustomIcons: TStyledDialogIcons);
begin
  UnregisterCustomIcons;
  _CustomIcons := ACustomIcons;
end;

procedure SetUseTitleInMessageDlg(AValue: boolean);
begin
  _UseTitleInMessageDlg := AValue;
end;

procedure RegisterCustomExecute(const AShowStyledTaskDialog: ITaskDialogLauncher;
  const AButtonFamily: TStyledButtonFamily = '');
begin
  _TaskDialogExecute := AShowStyledTaskDialog;
end;

procedure UnRegisterCustomExecute;
begin
  _TaskDialogExecute := nil;
end;

function GetTaskDlgType(
  const AIcon: TTaskDialogIcon): TMsgDlgType;
begin
  if AIcon = tdiNone then
    Result := TMsgDlgType.mtCustom
  else if AIcon = tdiWarning then
    Result := TMsgDlgType.mtWarning
  else if AIcon = tdiError then
    Result := TMsgDlgType.mtError
  else if AIcon = tdiInformation then
    Result := TMsgDlgType.mtInformation
  else if AIcon = tdiQuestion then
    Result := TMsgDlgType.mtConfirmation
  else if AIcon = tdiShield then
    Result := TMsgDlgType.mtCustom
  else
    Result := TMsgDlgType.mtInformation;
end;

function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx,
    -1, -1, _AutoClickDelay, _UseCommandLinks);
end;

function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton,
    -1, -1, _AutoClickDelay, _UseCommandLinks, '');
end;

function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y,
    _AutoClickDelay, _UseCommandLinks, '');
end;

function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y,
    _AutoClickDelay, _UseCommandLinks, '');
end;

function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1): Integer;
begin
  Result := StyledTaskMessageDlgPos(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton, X, Y);
end;

function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer;
var
  MsgWithTitle: string;
begin
  if Title <> '' then
    MsgWithTitle := UpperCase(Title)+sLineBreak+Msg
  else
    MsgWithTitle := Msg;
    Result := StyledMessageDlgPos(MsgWithTitle, DlgType, Buttons, HelpCtx, DefaultButton, -1, -1);
end;

function StyledTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; DefaultButton: TMsgDlgBtn; CustomButtonCaptions: array of string): Integer;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx,
    X, Y, HelpFileName, DefaultButton, -1, _UseCommandLinks, False, [], []);
end;

procedure StyledShowMessage(const Msg: string); overload;
begin
  StyledMessageDlg(Msg, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

procedure StyledShowMessageFmt(const Msg: string; Params: array of const); overload;
begin
  StyledMessageDlg(Format(Msg, Params), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

{ TStyledTaskDialog }

constructor TStyledTaskDialog.Create(AOwner: TComponent);
begin
  inherited;
  //Default Position: -1, -1 (is not a custom position)
  FPosition := TPoint.Create(-1,-1);
  FDialogButtonsFamily := _DefaultButtonsFamily;
  FDialogButtonsDrawType := _DefaultButtonsDrawType;
  FDialogButtonsRadius := _DefaultButtonsRadius;
  FDialogButtonsRoundedCorners := _DefaultButtonsRoundedCorners;
  UseCommandLinks := _UseCommandLinks;
  FUseTitleInMessageDlg := _UseTitleInMessageDlg;
  AlphaBlendValue := _AlphaBlendValue;
  ButtonsWidth := _ButtonsWidth;
  ButtonsHeight := _ButtonsHeight;
  FAutoClick := _AutoClickDelay > 0;
  if FAutoClick then
    FAutoClickDelay := _AutoClickDelay
  else
    FAutoClickDelay := DEFAULT_AUTOCLICK_DELAY;
  FMainIconSize := DEFAULT_MAIN_ICON_SIZE;
  DoOnDialogCreated;
end;

function TStyledTaskDialog.DoExecute(ParentWnd: HWND): Boolean;
var
  LTaskDlgType: TMsgDlgType;
begin
  LTaskDlgType := GetTaskDlgType(MainIcon);
  //Use a custom interface for StyledTaskDialog, if registered
  if Assigned(_TaskDialogExecute) then
    Result := _TaskDialogExecute.DoExecute(ParentWnd,
      LTaskDlgType, Self, Self.FDialogButtonsFamily)
  else
    Result := inherited DoExecute(ParentWnd);
end;

procedure TStyledTaskDialog.DoOnExpandButtonClicked(Expanded: Boolean);
begin
  inherited DoOnExpandButtonClicked(not Expanded);
end;

procedure TStyledTaskDialog.DoOnHelp;
var
  LHelpFile: string;
  LHelpSystem: IHelpSystem;
begin
  if HelpContext <> 0 then
  begin
    if FHelpFile = '' then
      LHelpFile := Application.HelpFile
    else
      LHelpFile := HelpFile;
    if System.HelpIntfs.GetHelpSystem(LHelpSystem) then
    try
      LHelpSystem.Hook(Application.Handle, LHelpFile, HELP_CONTEXT, HelpContext);
    except
      on E: Exception do
        ShowHelpException(E);
    end;
  end;
end;

procedure TStyledTaskDialog.DoOnHyperlinkClicked(const AURL: string);
begin
  inherited DoOnHyperlinkClicked(AURL);
end;

procedure TStyledTaskDialog.DoOnRadioButtonClicked(ButtonID: Integer);
begin
  inherited DoOnRadioButtonClicked(ButtonID);
end;

function TStyledTaskDialog.Execute(ParentWnd: HWND): Boolean;
begin
  FParentWnd := ParentWnd;
  Result := inherited Execute(ParentWnd);
end;

function TStyledTaskDialog.FindDialogButton(
  const AModalResult: TModalResult): TStyledButton;
begin
  if Assigned(FOnFindDialogButton) then
    Result := FOnFindDialogButton(AModalResult)
  else
    Result := nil;
end;

function TStyledTaskDialog.GetHandle: HWND;
begin
  Result := inherited Handle;
end;

function TStyledTaskDialog.GetUseCommandLinks: Boolean;
begin
  Result := TTaskDialogFlag.tfUseCommandLinks in Flags;
end;

function TStyledTaskDialog.IsCustomPosition: Boolean;
begin
  Result := (FPosition.X <> -1) and (FPosition.Y <> -1);
end;

function TStyledTaskDialog.IsDefaultFamily: Boolean;
begin
  Result := FDialogButtonsFamily <> _DefaultButtonsFamily;
end;

function TStyledTaskDialog.IsStoredDrawType: Boolean;
begin
  Result := FDialogButtonsDrawType <> _DefaultButtonsDrawType;
end;

function TStyledTaskDialog.IsStoredRadius: Boolean;
begin
  Result := FDialogButtonsRadius <> _DefaultButtonsRadius;
end;

function TStyledTaskDialog.IsStoredRoundedCorners: Boolean;
begin
  Result := FDialogButtonsRoundedCorners <> _DefaultButtonsRoundedCorners;
end;

class procedure TStyledTaskDialog.RegisterDefaultRenderingStyle(
  const ADrawType: TStyledButtonDrawType;
  const AFamily: TStyledButtonFamily;
  const ARadius: Integer;
  const ARoundedCorners: TRoundedCorners);
begin
  _DefaultButtonsFamily := AFamily;
  _DefaultButtonsDrawType := ADrawType;
  _DefaultButtonsRadius := ARadius;
  _DefaultButtonsRoundedCorners := ARoundedCorners;
end;

procedure TStyledTaskDialog.SetAlphaBlendValue(const AValue: Byte);
begin
  FAlphaBlendValue := AValue;
end;

procedure TStyledTaskDialog.SetUseAnimationInverse(const AValue: Boolean);
begin
  if FUseAnimationInverse <> AValue then
  begin
    if AValue then
      UseAnimations := True;
    FUseAnimationInverse := AValue;
  end;
end;

procedure TStyledTaskDialog.SetUseAnimationLoop(const AValue: Boolean);
begin
  if FUseAnimationLoop <> AValue then
  begin
    if AValue then
      UseAnimations := True;
    FUseAnimationLoop := AValue;
  end;
end;

procedure TStyledTaskDialog.SetAutoClick(const AValue: Boolean);
begin
  FAutoClick := AValue;
end;

procedure TStyledTaskDialog.SetAutoClickDelay(const AValue: Integer);
begin
  FAutoClickDelay := AValue;
end;

procedure TStyledTaskDialog.SetButtonsHeight(const AValue: Integer);
begin
  CheckValue('TStyledTaskDialog.ButtonsHeight', AValue,
    MIN_STYLEDDIALOG_BUTTONSSIZE, MAX_STYLEDDIALOG_BUTTONSHEIGHT);
  FButtonsHeight := AValue;
end;

procedure TStyledTaskDialog.SetButtonsWidth(const AValue: Integer);
begin
  CheckValue('TStyledTaskDialog.SetButtonsWidth', AValue,
    MIN_STYLEDDIALOG_BUTTONSSIZE, MAX_STYLEDDIALOG_BUTTONSWIDTH);
  FButtonsWidth := AValue;
end;

procedure TStyledTaskDialog.SetHandle(const AValue: HWND);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Self.ClassType);
    Field := RttiType.GetField('FHandle');
    if Assigned(Field) then
      Field.SetValue(Self, AValue);
  finally
    Context.Free;
  end;
end;

procedure TStyledTaskDialog.SetMainIconSize(const AValue: Integer);
begin
  if AValue < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  FMainIconSize := AValue;
end;

procedure TStyledTaskDialog.SetUseAnimations(const AValue: Boolean);
begin
  if FUseAnimations <> AValue then
  begin
    if AValue and not AnimatedTaskDialogFormRegistered then
      raise EStyledTaskDialogException.CreateFmt(
        ERR_DIALOG_FORM_NOT_REGISTERED,
        ['Skia.Vcl.StyledTaskDialogAnimatedUnit.pas']);
    FUseAnimations := AValue;
  end;
end;

procedure TStyledTaskDialog.SetUseCommandLinks(const AValue: Boolean);
begin
  if AValue then
    Flags :=  Flags + [TTaskDialogFlag.tfUseCommandLinks]
  else
    Flags :=  Flags - [TTaskDialogFlag.tfUseCommandLinks];
end;

function GetDialogTypeTitle(const DlgType: TMsgDlgType): string;
begin
  case DlgType of
    mtWarning      : Result := STR_WARNING;
    mtError        : Result := STR_ERROR;
    mtInformation  : Result := STR_INFORMATION;
    mtConfirmation : Result := STR_CONFIRM;
    mtCustom       : Result := STR_INFORMATION;
  end;
end;

function MsgDlgBtnToDefaultBtn(const ABtn: TMsgDlgBtn): TTaskDialogCommonButton;
begin
  case ABtn of
    mbYes: Result := tcbYes;
    mbNo: Result := tcbNo;
    mbOK: Result := tcbOk;
    mbCancel: Result := tcbCancel;
    mbRetry: Result := tcbRetry;
    mbClose: Result := tcbClose;
  else
    Result := tcbOk;
  end;
end;

initialization
  _DialogButtonsFamily := DEFAULT_CLASSIC_FAMILY;
  _UseCommandLinks := False;
  _UseTitleInMessageDlg := True;
  _AlphaBlendValue := DEFAULT_STYLEDDIALOG_ALPHABLEND;
  _ButtonsWidth := DEFAULT_STYLEDDIALOG_BUTTONSWIDTH;
  _ButtonsHeight := DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT;
  _DialogFont := nil;
  //Initialize TStyledTaskDialog class variables for default button styles
  TStyledTaskDialog._DefaultButtonsFamily := DEFAULT_CLASSIC_FAMILY;
  TStyledTaskDialog._DefaultButtonsDrawType := btRoundRect;
  TStyledTaskDialog._DefaultButtonsRadius := DEFAULT_RADIUS;
  TStyledTaskDialog._DefaultButtonsRoundedCorners := ALL_ROUNDED_CORNERS;
  RegisterTaskDialogFormClass(TStyledTaskDialogStd);

finalization
  if Assigned(_DialogFont) then
    _DialogFont.Free;
  UnRegisterTaskDialogFormClass(TStyledTaskDialogStd);

end.
