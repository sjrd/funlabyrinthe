unit JvThreadProgressDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvThread, JvThreadDialog, JvDynControlEngineIntf;

type
  TJvThreadProgressDialogOptions = class(TJvThreadSimpleDialogOptions)
  private
    FMinProgress: Integer;
    FMaxProgress: Integer;

    FProgressPosition: Integer;
  public
    constructor Create(AOwner: TJvCustomThreadDialog); override;
  published
    property MinProgress: Integer read FMinProgress write FMinProgress
      default 0;
    property MaxProgress: Integer read FMaxProgress write FMaxProgress
      default 100;

    property ProgressPosition: Integer
      read FProgressPosition write FProgressPosition default 0;
  end;

  TJvThreadProgressDialog = class(TJvThreadSimpleDialog)
  private
    function GetDialogOptions: TJvThreadProgressDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadProgressDialogOptions);
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
  public
    function CreateThreadDialogForm(
      ConnectedThread: TJvThread): TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadProgressDialogOptions
      read GetDialogOptions write SetDialogOptions;
  end;

  TJvThreadProgressDialogForm = class(TJvThreadSimpleDialogForm)
  private
    FProgressBar: TWinControl;

    function GetDialogOptions: TJvThreadProgressDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadProgressDialogOptions);
  protected
    procedure CreateFormControls;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadProgressDialogOptions
      read GetDialogOptions write SetDialogOptions;
  end;

implementation

{--------------------------------------}
{ TJvThreadProgressDialogOptions class }
{--------------------------------------}

constructor TJvThreadProgressDialogOptions.Create(
  AOwner: TJvCustomThreadDialog);
begin
  inherited;

  FMinProgress := 0;
  FMaxProgress := 100;
end;

{-----------------------------------}
{ TJvThreadProgressDialogForm class }
{-----------------------------------}

function TJvThreadProgressDialogForm.GetDialogOptions:
  TJvThreadProgressDialogOptions;
begin
  Result := TJvThreadProgressDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadProgressDialogForm.SetDialogOptions(
  Value: TJvThreadProgressDialogOptions);
begin
  inherited DialogOptions := Value;
end;

procedure TJvThreadProgressDialogForm.CreateFormControls;
var
  Panel: TWinControl;
  ITmpAlign: IJvDynControlAlign;
begin
  inherited CreateFormControls;

  (FindComponent('Progressbar') as TWinControl).Parent := nil;

  Panel := FindComponent('ProgressbarPanel') as TWinControl;
  FProgressbar := DynControlEngine.CreateProgressbarControl(Self, Panel,
    'Progressbar2');
  Panel.Height := FProgressbar.Height + DefaultBorderWidth*2;
  if Supports(FProgressbar, IJvDynControlAlign, ITmpAlign) then
    ITmpAlign.ControlSetAlign(alClient);
end;

procedure TJvThreadProgressDialogForm.UpdateFormContents;
var
  ITmpProgressbar: IJvDynControlProgressbar;
begin
  inherited;

  if Assigned(DialogOptions) then
  begin
    if Supports(FProgressbar, IJvDynControlProgressbar, ITmpProgressbar) then
    begin
      ITmpProgressbar.ControlSetMin(DialogOptions.MinProgress);
      ITmpProgressbar.ControlSetMax(DialogOptions.MaxProgress);
      ITmpProgressbar.ControlSetPosition(DialogOptions.ProgressPosition);
    end;
  end;
end;

{-------------------------------}
{ TJvThreadProgressDialog class }
{-------------------------------}

function TJvThreadProgressDialog.GetDialogOptions:
  TJvThreadProgressDialogOptions;
begin
  Result := TJvThreadProgressDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadProgressDialog.SetDialogOptions(
  Value: TJvThreadProgressDialogOptions);
begin
  (inherited DialogOptions).Assign(Value);
end;

function TJvThreadProgressDialog.CreateDialogOptions:
  TJvCustomThreadDialogOptions;
begin
  Result := TJvThreadProgressDialogOptions.Create(Self);
end;

function TJvThreadProgressDialog.CreateThreadDialogForm(
  ConnectedThread: TJvThread): TJvCustomThreadDialogForm;
var
  ThreadDialogForm: TJvThreadProgressDialogForm;
begin
  if DialogOptions.ShowDialog then
  begin
    ThreadDialogForm := TJvThreadProgressDialogForm.CreateNewFormStyle(
      ConnectedThread, DialogOptions.FormStyle);
    ThreadDialogForm.DialogOptions := DialogOptions;
    ThreadDialogForm.OnPressCancel := OnPressCancel;
    ThreadDialogForm.ChangeThreadDialogOptions := ChangeThreadDialogOptions;
    ThreadDialogForm.CreateFormControls;
    Result := ThreadDialogForm;
  end else
    Result := nil;
end;

end.

