{*
  Constantes globales de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyCoreConsts;

interface

const {don't localize}
  CurrentVersion = '5.0';  /// Version courante de FunLabyrinthe
  CurrentMinorVersion = 2; /// Version mineure courante de FunLabyrinthe
  FunLabyAuthorName = 'S�bastien Jean Robert Doeraene'; /// Auteur
  FunLabyAuthorEMail = 'sjrd@redaction-developpez.com'; /// E-mail de l'auteur
  FunLabyWebSite = 'http://funlabyrinthe.game-host.org/'; /// Site

resourcestring
  // Noms de composants
  SDefaultObjectInfos = '%s : %d';
  SNothing = 'Rien';
  SEffectName = '%1:s sur %0:s';
  SToolName = '%s avec %s';
  SObstacleName = '%s obstru� par %s';
  SWhichObject = 'Quel objet voulez-vous utiliser ?';
  SDefaultPlayerName = 'Joueur';
  SComponentCreatorHint = 'Cr�er un nouveau composant de ce type';

  // Noms des cat�gories standard
  SCategoryFields = 'Terrains';
  SCategoryEffects = 'Effets';
  SCategoryTools = 'Outils';
  SCategoryObstacles = 'Obstacles';
  SCategoryNeutrals = 'Neutres';
  SCategoryVehicles = 'V�hicules';
  SCategoryPlayers = 'Joueurs';
  SCategoryObjects = 'Objets';
  SCategoryPlugins = 'Plug-in';
  SCategoryComponentCreators = 'Composants suppl�mentaires';

  // Erreurs de composants
  SComponentNotFoundTitle = 'Composant non trouv�';
  SComponentNotFound = 'Le composant d''ID %s n''existe pas';
  SInvalidComponentTitle = 'Composant invalide';
  SInvalidComponent = 'Le composant d''ID %s n''est pas valide';
  SRepareComponentError =
    'Voulez-vous ignorer cette erreur et tenter de continuer ?';
  SUnsupportedCommand = 'La commande %s n''est pas support�e';
  STemporaryStatedMap =
    'La carte d''ID %s est dans un �tat temporaire qui ne peut �tre enregistr�';

  // Titres des messages
  SDescription = 'Description';
  SMessage = 'Message';
  STip = 'Indice';
  SChoice = 'Choix';
  SError = 'Erreur';
  SFailure = '�chec';
  SBlindAlley = 'Impasse';
  SWon = 'Gagn� !';
  SLost = 'Perdu !';

  // Formats de fichier et traitement des fichiers
  SFullVersionNumber = '%s (build %d)';
  SInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  SVersionTooHigh = 'Le fichier a �t� enregistr� avec une version ult�rieure '+
    'de FunLabyrinthe (v%s). Il ne peut �tre ouvert.';
  SFileNotFound = 'Le fichier sp�cifi� "%s" n''existe pas';
  SUnknownUnitType = 'Type d''unit� ''%s'' inconnu';
  SThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  SEditingNotAllowed = 'L''�dition de ce fichier n''est pas permise';
  SCantEditSaveguard = 'L''�dition d''une sauvegarde est impossible';
  SSourcesNotHandledWhilePlaying =
    'Les fichiers source ne sont pas g�r�s en mode jeu';
  SNoFileName = 'Aucun nom de fichier sp�cifi�';
  SCantLoadPackage = 'Impossible de charger le paquet "%s"';

implementation

end.

