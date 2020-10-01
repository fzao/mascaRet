Module M_MY_GLOBAL_VAR_SED

  use M_PRECISION

  implicit none

  integer :: NIteSed = 0                           ! Nombre d'iterations de calcul sedimentaire
!  real(DOUBLE) :: fracH = 0.05D0                   ! MS2018 ==> devient un mot-cle
  real(DOUBLE) :: fracH                            ! Pourcentage pour le critere de planimetrage
  real(DOUBLE) :: absolute_clip                    ! clip absolu en cas de desactivation du clipping
  logical      :: optionPente                      ! MS2018 choix entre une pente locales ou non
  real(DOUBLE) :: dm, d16, d84                     ! MS2018 diametre moyen, d16 et d84 pour lefort et recking
  real(DOUBLE), dimension(:), pointer :: Vsed => null()      ! Volume de sediments transportes
  real(DOUBLE), dimension(:), pointer :: Hsed => null()      ! Epaisseur de sediments deposes ou erodes
  real(DOUBLE), dimension(:), pointer :: myZsl => null()     ! Cote de la surface libre
  real(DOUBLE), dimension(:), pointer :: mySm => null()      ! Surface mouillee
  real(DOUBLE), dimension(:,:), pointer :: DeltaH => null()
  logical      :: bedload_option                   ! bedload option
  logical      :: suspension_option                ! bedload option
  logical      :: sediment_slide_option            ! sediment slide option
  logical      :: clipping_option                  ! planim clipping option

End Module M_MY_GLOBAL_VAR_SED

