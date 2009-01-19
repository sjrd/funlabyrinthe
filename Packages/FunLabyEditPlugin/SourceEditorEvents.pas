unit SourceEditorEvents;

interface

uses
  SourceEditors;

type
  {*
    �v�nements d'activation et de d�sactivation de l'�diteur
    @author sjrd
    @version 5.0
  *}
  IActivatingSourceEditor = interface(ISourceEditor50)
    ['{45DDA58D-2971-44BA-B42B-8AE9D2F66C9B}']

    {*
      Active l'�diteur
      Cette m�thode est appel�e lorsque l'onglet contenant cet �diteur est
      affich�, et ce apr�s que le contr�le a �t� rendu visible.
    *}
    procedure Activate;

    {*
      D�sactive l'�diteur
      Cette m�thode est appel�e lorsque l'onglet contenant cet �diteur va �tre
      cach�, et ce avant que le contr�le ait �t� rendu invisible.
    *}
    procedure Deactivate;
  end;

implementation

end.

