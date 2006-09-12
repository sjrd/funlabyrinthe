unit FunLabyEditMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, Menus, ImgList, StdCtrls,
  ExtCtrls, Tabs, ComCtrls, ActnMenus, ToolWin, ActnCtrls;

type
  TFormMain = class(TForm)
    Images: TImageList;
    ActionManager: TActionManager;
    ActionExit: TAction;
    ToolBarFile: TActionToolBar;
    MainMenuBar: TActionMainMenuBar;
    StatusBar: TStatusBar;
    PanelScrews: TPanel;
    PanelPlayers: TPanel;
    PanelCenter: TPanel;
    MapTabSet: TTabSet;
    ImageMap: TImage;
    SplitterScrews: TSplitter;
    SplitterPlayers: TSplitter;
    HorzScrollBar: TScrollBar;
    VertScrollBar: TScrollBar;
    LabelPosition: TLabel;
    LabelField: TLabel;
    LabelEffect: TLabel;
    LabelObstacle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure DoNothing(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.DoNothing(Sender: TObject);
begin
  {don't delete this comment!}
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  WindowState := wsMaximized;
end;

end.

