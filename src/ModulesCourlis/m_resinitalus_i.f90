Module M_ResIniTalus_I

Interface

Subroutine ResIniTalus  (  &

  ProfilCourlis    ,  & ! Profils sedimentaires
  Talus        ,  & ! Paramètres relatifs aux talus
  CoucheSed      ,  & ! Paramètres sédimentaires des différentes couches
  ResIni        ,  & ! Resistance initiale des blocs au mouvement
  Erreur        )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Calcule de la résistance initiale des colonnes sédimentaires
!  --------    au mouvements
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C      ! Definition des constante tq EPS*, W0, ...
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info

use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS
use M_COUCHE_T        ! Définition du type COUCHE_T
use M_TALUS_T        ! Définition du type TALUS_T

use M_ERREUR_T        ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur


!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(PROFIL_COURLIS_T), dimension(:)  , intent(in   )  :: ProfilCourlis
  type(COUCHE_T)    , dimension(:)  , intent(in   )  :: CoucheSed
  type(TALUS_T)              , intent(in   )  :: Talus

! Variables de sortie
  real(DOUBLE)      , dimension(:,:), intent(  out)  :: ResIni

! Variables locales
! PU2017 : Mise en commentaire des var loc
!  integer :: i, j, k        ! Compteurs
!  integer :: NProfil        ! Nombre de profils
!  integer :: NInt          ! Nombre d'interfaces sédimentaires
!  integer :: NPt          ! Nombre de points décrivant le profil en travers

!  real(DOUBLE) :: HsG, HsD, HsM    ! Hauteurs de la colonne sédimentaire (gauche, droite, moyenne)
!  real(DOUBLE) :: HsGC, HsDC, HsMC  ! Hauteurs de la couche en cours
!  real(DOUBLE) :: Dx        ! Pas de discretisation horizontal
!  real(DOUBLE) :: DzS, DxzS      ! Pas de discretisation vertical, hypothenuse a la surface du bloc
!  real(DOUBLE) :: DzF, DxzF     ! Pas de discretisation vertical, hypothenuse du fond du bloc
!  real(DOUBLE) :: SinS, CosS, SinF, CosF  ! Angles du bloc

!  real(DOUBLE), dimension(:,:), allocatable :: Z      ! Cotes des interfaces sédimentaires

! Traitement des erreurs
! PU2017 : Mis en commentaire des var loc pour traitement erreur
  type(ERREUR_T), intent(inout) :: Erreur
!  integer            :: retour  ! Code de retour de la fonction read, allocate
!  character(132) :: arbredappel_old      ! Ancien arbre d'appel

! Constantes

!=========================================================================


End Subroutine ResIniTalus

End Interface

End Module M_ResIniTalus_I



