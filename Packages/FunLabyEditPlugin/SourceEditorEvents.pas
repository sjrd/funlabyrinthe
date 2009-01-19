unit SourceEditorEvents;

interface

uses
  SourceEditors;

type
  {*
    Événements d'activation et de désactivation de l'éditeur
    @author sjrd
    @version 5.0
  *}
  IActivatingSourceEditor = interface(ISourceEditor50)
    ['{45DDA58D-2971-44BA-B42B-8AE9D2F66C9B}']

    {*
      Active l'éditeur
      Cette méthode est appelée lorsque l'onglet contenant cet éditeur est
      affiché, et ce après que le contrôle a été rendu visible.
    *}
    procedure Activate;

    {*
      Désactive l'éditeur
      Cette méthode est appelée lorsque l'onglet contenant cet éditeur va être
      caché, et ce avant que le contrôle ait été rendu invisible.
    *}
    procedure Deactivate;
  end;

implementation

end.

