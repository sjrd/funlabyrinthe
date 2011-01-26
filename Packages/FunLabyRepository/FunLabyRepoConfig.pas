{*
  Fenêtre de configuration du repository
  @author sjrd
  @version 5.2
*}
unit FunLabyRepoConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, StdCtrls, CheckLst, ComCtrls;

type
  {*
    Fenêtre de configuration du repository
    @author sjrd
    @version 5.2
  *}
  TFormRepoConfig = class(TForm)
    PageControl: TPageControl;
    TabSubscriptions: TTabSheet;
    PanelBottom: TPanel;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ListBoxSources: TCheckListBox;
    EditPseudo: TLabeledEdit;
    EditName: TLabeledEdit;
    LinkLabelPresentationURL: TLinkLabel;
    ButtonUpdateSourceList: TButton;
    ButtonPull: TBitBtn;
  private
    function InternalEditConfiguration: Boolean;
  public
    class function EditConfiguration: Boolean;
  end;

implementation

{$R *.dfm}

{-----------------------}
{ TFormRepoConfig class }
{-----------------------}

{*
  Édite la configuration du repository
  @return True si une recompilation des unités est nécessaire
*}
function TFormRepoConfig.InternalEditConfiguration: Boolean;
begin
  Result := ShowModal = mrOK;
end;

{*
  Édite la configuration du repository
  @return True si une recompilation des unités est nécessaire
*}
class function TFormRepoConfig.EditConfiguration: Boolean;
begin
  with Create(Application) do
  try
    Result := InternalEditConfiguration;
  finally
    Free;
  end;
end;

end.

