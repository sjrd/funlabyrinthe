unit FunLabyEditConsts;

interface

resourcestring
  SErrorTitle = 'Erreur';
  SFatalErrorTitle = 'Erreur fatale';

  SBaseSepiRootLoadError =
    'Erreur au chargement des fonctionnalit�s coeur de FunLabyrinthe avec le '+
    'message "%s". FunLabyrinthe ne peut continuer et doit fermer.';

  SDefaultPlayerName = 'Joueur';

  SConfirmExitTitle = 'Enregistrer le fichier';
  SConfirmExit = 'Le fichier a �t� modifi�. Voulez-vous l''enregistrer ?';

  SProjectAlreadyExists = 'Un projet avec ce nom de fichier existe d�j�.';

  SCantSaveTitle = 'Impossible d''enregistrer';
  SCantSaveBadProjectFileName = 'Vous devez s�lectionner un fichier '+
    '''<Mon Projet>.flp'' dans le dossier ''Projects\<Mon Projet>\';

  SAutoCompileSuccessTitle = 'Succ�s';
  SAutoCompileSuccess =
    'La recompilation de toutes unit�s a �t� faite avec succ�s.';

  SOnlyFieldOutsideTitle = 'Remplacer l''ext�rieur de la carte';
  SOnlyFieldOutside =
    'Seuls les terrains sont accept�s en dehors de la carte';

  SRemoveMapTitle = 'Supprimer une carte';
  SRemoveMap =
    '�tes-vous certain de vouloir supprimer compl�tement cette carte ?';

  SRemoveSourceTitle = 'Retirer un fichier source';
  SRemoveSource =
    '�tes-vous certain de vouloir retirer ce fichier source du projet ?';
  SErrorWhileOpeningSourceFile =
    'Une erreur est survenu lors de l''ouverture du fichier avec le message '+
    '"%s". Voulez-vous retirer ce fichier source du projet ?';

  SComponentDataTitle = 'Propri�t�s du composant';
  SPlayerDataTitle = 'Propri�t�s par joueur';
  SPlayerAttributesTitle = 'Attributs du joueur';
  SPlayerPluginsTitle = 'Plug-in attach�s au joueur';

  SCollectionItemTitle = '%d - %s';

  SInvalidFileNameTitle = 'Nom de fichier invalide';
  SInvalidUnitName =
    '%s n''est pas un nom d''unit� valide, il ne doit contenir que des '+
    'lettres non accentu�es et des chiffres, et commencer par une lettre';

  SNoFieldTitle = 'Aucun terrain';
  SNoField =
    'Impossible de lancer cette op�ration car ce labyrinthe ne contient '+
    'aucun terrain';

  SResizeErrorTitle = 'Erreur de redimensionnement';
  SCantResizeToEmpty = 'Impossible de r�tr�cir plus la carte';

  SConfirmDeleteComponentTitle = 'Supprimer le composant';
  SConfirmDeleteComponent =
    '�tes-vous s�r-e de vouloir supprimer ce composant ?';

implementation

end.

