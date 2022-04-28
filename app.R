library(shiny)
#library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(shinyjs)
#Nodo
library(data.tree)
#Otros
################################################# UI #################################################

## Variables globales ##

cant_por_Seleccionar = 40
cont_nodos <<- 0
niveles_gamemode = readRDS("nvGamemode.RDS")
niveles_maps = readRDS("nvMaps.RDS")
data_Keys_Heroes = readRDS("dataKeysHeroes.RDS")
data_Keys_Maps = readRDS("dataKeysMaps.RDS")

## Descarga de los modelos a usar ##

LstGrndModel = NULL
for(i in 1:(length(niveles_maps))){
  LstGrndModel = append(LstGrndModel, list(readRDS(paste0("GrndModel", i, ".RDS"))))}

######## FUNCTIONS ########

## Funciones de tablero ##

crear_tablero = function(tablero, gamemode = "8", mapid = "1001"){
  #¿El gamemode es invalido?
  if(!(gamemode %in% niveles_gamemode)){
    stop("El gamemode ingresado no esta permitido")}
  #¿El mapid es invalido?
  if(!(mapid %in% niveles_maps)){
    stop("El mapid ingresado no esta permitido")}
  if(missing(tablero)){
    #Retornar lista con [turno, baneos, heroes, gamemode, mapid]
    return(list(1, c(), c(), gamemode, mapid))}
  else{
    return(tablero)}
}
realizar_movimiento = function(tablero, heroe){
  #Falta ingresar tablero
  if(missing(tablero)){
    stop("Falta ingresar tablero")}
  #¿El heroe es invalido?
  if(missing(heroe) || !(heroe %in% data_Keys_Heroes$primaryname) || (heroe %in% tablero[[2]]) || (heroe %in% tablero[[3]])){
    stop("El heroe es invalido")}
  else{
    #Si estamos en un turno de baneo:
    if(tablero[[1]] == 1 || tablero[[1]] == 2 || tablero[[1]] == 8 || tablero[[1]] == 9){
      #agrego heroe a la lista de baneos
      tablero[[2]] = append(tablero[[2]], heroe)
      #pasar al siguiente turno
      tablero[[1]] = tablero[[1]]+1}
    #Si los turnos de seleccion no han terminado
    else if(tablero[[1]] < 15){
      #Se agrega el heroe a la lista de heroes seleccionados
      tablero[[3]] = append(tablero[[3]], heroe)
      #pasar al siguiente turno
      tablero[[1]] = tablero[[1]]+1}
    else{
      stop("Turno no valido, el juego posiblemente a terminado.")}
    return(tablero)}
}
is_done = function(tablero){
  if(missing(tablero)){
    stop("Tablero no ingresado")}
  else{
    #Si el turno es igual a 15
    if(tablero[[1]] == 15){
      #Si hay 4 nombres en la lista de baneos
      if(length(tablero[[2]]) == 4){
        #Si hay 10 nombres en la lista de seleccionados
        if(length(tablero[[3]]) == 10){
          return(TRUE)}
        else{
          return(FALSE)}}
      else{
        return(FALSE)}}
    else{
      return(FALSE)}}
}
#Retorna un vector con el nombre de los heroes no utilizados
set_movimientos = function(tablero){
  #Creacion de lista preliminar de de heroes disponibles a usar
  lista_heroes_disponibles = data_Keys_Heroes$primaryname
  #Quitar los heroes ya baneados
  for (heroe_baneado in tablero[[2]]){
    lista_heroes_disponibles = lista_heroes_disponibles[which(lista_heroes_disponibles != heroe_baneado)]}
  #Quitar los heroes ya escogidos
  for (heroe_seleccionado in tablero[[3]]){
    lista_heroes_disponibles = lista_heroes_disponibles[which(lista_heroes_disponibles != heroe_seleccionado)]}
  #Retornar la lista de heroes no utilizados
  return(lista_heroes_disponibles)
}

## Funcion de nodo ##

crear_nodo = function(nombre_nodo, tablero_entrada){
  if(is_done(tablero_entrada)){
    tmp_root = Node$new(nombre_nodo, tablero=tablero_entrada, is_terminal=T, visitas=0, puntuacion=0, is_extended=T)}
  else{
    tmp_root = Node$new(nombre_nodo, tablero=tablero_entrada, is_terminal=F, visitas=0, puntuacion=0, is_extended=F)}
  return(tmp_root)
}

## MCTS ##

search = function(actual_node, constante, booleano_politica = TRUE){
  #Tiempo, en segundos, que se permite iterar
  period = 20
  #Guardar tiempo actual
  actual_time = Sys.time()  ## starting data & time
  #(Tiempo) o (numero de iteraciones) que puede realizar el simulador MCTS
  while (difftime(Sys.time(), actual_time, units = "secs")[[1]] < period){
    tmp_nodo = select(actual_node, constante, booleano_politica)
    tmp_puntuacion = rollout(tmp_nodo$tablero, booleano_politica)
    backpropagate(tmp_nodo, tmp_puntuacion)}
  retorno = tryCatch(get_best_move(actual_node, 0, booleano_politica), error = function(c){
    print("Error al realizar el movimiento, intentelo denuevo")
    return(actual_node)})
  return(retorno)
}
#Selecionar el nodo más prometedor
select = function(nodo, constante, booleano_politica = TRUE){
  #Asegurarse de que estamos en un nodo no terminal
  while(!(nodo$is_terminal)){
    #Si el nodo esta completamente extendido:
    if(nodo$is_extended){
      nodo = get_best_move(nodo, constante, booleano_politica)}
    #Si el nodo NO esta completamente extendido
    else{
      return(expand(nodo, booleano_politica))}}
  return(nodo)
}
tree_policy = function(lista_estados){
  #creacion de un vector vacio que guardara los pesos del
  #heroe elejido en cada estado
  pesos = NULL
  #Creacion de variable que tendra el peso total de todos los pesos.
  peso_total = 0
  #Pasar por cada heroe posible
  for (estado_actual in lista_estados){
    tmp_peso = data_Keys_Heroes[which(data_Keys_Heroes$primaryname == estado_actual),4]
    pesos = append(pesos, tmp_peso)
    peso_total = peso_total + tmp_peso}
  #elejir un numero aleatorio entre 1 al peso_total
  peso_aleatorio = sample(1:(peso_total), 1)
  #Crear una variable numerica que almacena la direccion del estado a elegir
  direccion = 1
  #Ir por cada peso del vector para ver que jugara se elige
  for (peso_actual in pesos){
    #Si el peso aleatorio es inferior al peso actual
    if(peso_aleatorio <= peso_actual){
      #Retornar estado en la direccion correspondiente
      return(lista_estados[direccion])}
    #Disminuir el peso aleatorio por el peso actual
    peso_aleatorio = peso_aleatorio - peso_actual
    #aumentar la direccion apropiadamente
    direccion = direccion + 1}
  #No se deberia llegar a esta parte
  stop("El peso aleatorio es mayor que el peso total")
}
expand = function(nodo, booleano_politica = TRUE){
  #Se construye un vector de posibles heroes a seleccionar
  estados = set_movimientos(nodo$tablero)
  #Se obtiene la cantidad total de movimientos disponibles en este estado
  #Por cada estado se "planea" generar un nodo hijo
  while(length(estados) != 0){
    #Se selecciona un estado actual dependiendo de si la politica
    #esta activada o no
    if(booleano_politica){
      estado_actual = tree_policy(estados)}
    else{
      estado_actual = estados[(sample(1:(length(estados)), 1))]}
    #Variable booleana que indica si el estado actual esta o no en los hijos del nodo
    estado_en_hijos = FALSE
    #Se crea un tablero con la seleccion del heroe realizada
    tablero_con_estado_actual = realizar_movimiento(nodo$tablero, estado_actual)
    #Se confirma que el estado no existe previamente
    for(nodo_hijo_actual in nodo$children){
      if(identical(tablero_con_estado_actual, nodo_hijo_actual$tablero)){
        estado_en_hijos = TRUE}}
    #Si el estado no esta en los hijos del nodo
    if(!estado_en_hijos){
      #Agregar un nodo hijo que tenga el estado
      nuevo_nodo = crear_nodo(cont_nodos, tablero_con_estado_actual)
      nodo$AddChildNode(nuevo_nodo)
      cont_nodos <<- cont_nodos+1
      #Si se alcanzaron todos los estados
      if(length(nodo$children) == cant_por_Seleccionar){
        nodo$is_extended = TRUE}
      #Una vez se agrega un nodo hijo, nos retiramos
      return(nuevo_nodo)}
    #Elimino el estado actual del vector de estados.
    estados = estados[which(estados != estado_actual)]}
  #Debugging
  print("No se deberia de llegar aqui")
}
#Recibe: Una lista de nombres de heroes
#Retorna: un nombre de heroe escogido segun la polotica.
default_policy = function(lista_estados){
  #creacion de un vector vacio que guardara los pesos del heroe elejido en cada estado
  pesos = NULL
  #Creacion de variable que tendra el peso total de todos los pesos.
  peso_total = 0
  #Pasar por cada heroe posible
  for (estado_actual in lista_estados){
    tmp_peso = data_Keys_Heroes[which(data_Keys_Heroes$primaryname == estado_actual),4]
    pesos = append(pesos, tmp_peso)
    peso_total = peso_total + tmp_peso}
  #elejir un numero aleatorio entre 1 al peso_total
  peso_aleatorio = sample(1:(peso_total), 1)
  #Crear una variable numerica que almacena la direccion del estado a elegir
  direccion = 1
  #Ir por cada peso del vector para ver que jugara se elige
  for (peso_actual in pesos){
    #Si el peso aleatorio es inferior al peso actual
    if(peso_aleatorio <= peso_actual){
      #Retornar estado en la direccion correspondiente
      return(lista_estados[direccion])}
    #Disminuir el peso aleatorio por el peso actual
    peso_aleatorio = peso_aleatorio - peso_actual
    #aumentar la direccion apropiadamente
    direccion = direccion + 1}
  #No se deberia llegar a esta parte
  stop("El peso aleatorio es mayor que el peso total")
}
#Simula el juego mediante una politica de Rollout hasta llegar a un final de juego
rollout = function(tablero, booleano_politica = TRUE){
  #Mientras no se llegue a un final...
  while(!is_done(tablero)){
    #Intentar conseguir el set de movimientos permitidos
    tmp_estados = set_movimientos(tablero)
    ############realizar un movimiento segun la politica#########
    #Se selecciona un tablero dependiendo de si la politica esta activada o no
    if(booleano_politica){
      #Los heroes con mayor probabilidad de victoria individual
      #tienen más probabilidad de ser seleccionados
      tablero = realizar_movimiento(tablero, default_policy(tmp_estados))}
    else{
      #Aleatorio
      tablero = realizar_movimiento(tablero, tmp_estados[sample(1:(length(tmp_estados)), 1)])}}
  ###############Creacion de la instancia a predecir##################
  ##Genero un vector con los ID de cada heroe seleccionado
  #generar vector vacio
  tmp_vector_IDs = c()
  for(nombre in tablero[[3]]){
    tmp_vector_IDs = append(tmp_vector_IDs, which(data_Keys_Heroes[,3] == nombre))}
  #Agregar un factor
  slq_draft = data.frame(as.factor(0))
  for (i in 1:(length(data_partidas)-1)){
    #Editar gamemode en instancia nueva
    if(i == 1){
      #Se establecen los nombres de los niveles para que concuerden con todas las instancias de consuta
      levels(slq_draft[,i]) = niveles_gamemode
      #darle el valor de gamemode
      slq_draft[,i][1] = as.character(tablero[[4]])}
    #Editar mapid en instancia nueva
    else if(i == 2){
      #Agregar un factor
      slq_draft = data.frame(slq_draft, as.factor(0))
      #Se establecen los nombres de los niveles para que concuerden
      #con todas las instancias de consuta
      levels(slq_draft[,i]) = niveles_maps
      #darle el valor de mapid
      slq_draft[,i][1] = as.character(tablero[[5]])}
    #Editar heroes en instancia nueva
    else{
      #Agregar un factor
      slq_draft = data.frame(slq_draft, as.factor(0))
      #Se establecen los nombres de los niveles para que concuerden con todas las instancias de consuta
      levels(slq_draft[,i]) = c("0", "1")
      if((i-2) <= cant_Heroes){
        if((i-2) == tmp_vector_IDs[1] || (i-2) == tmp_vector_IDs[4] || (i-2) == tmp_vector_IDs[5] || (i-2) == tmp_vector_IDs[8] || (i-2) == tmp_vector_IDs[9]){
          #darle el valor que simboliza si estuvo o no seleccionado el personaje
          slq_draft[,i][1] = "1"}}
      else{
        if((i-(cant_Heroes+2)) == tmp_vector_IDs[2] || (i-(cant_Heroes+2)) == tmp_vector_IDs[3] || (i-(cant_Heroes+2)) == tmp_vector_IDs[6] || (i-(cant_Heroes+2)) == tmp_vector_IDs[7] || (i-(cant_Heroes+2)) == tmp_vector_IDs[10]){
          #darle el valor que simboliza si estuvo o no seleccionado el personaje
          slq_draft[,i][1] = "1"}}}}
  slq_draft = setNames(slq_draft, append(append(c("gamemode", "mapid"), data_Keys_Heroes$primaryname), paste(data_Keys_Heroes$primaryname, "_02", sep = "")))
  #Prediccion de victoria
  puntuacion_victoria = round(predict(Grnd_Modelo_por_mapa[[which(niveles_maps == tablero[[5]])]], slq_draft[,-c(1,2)],n.trees = as.numeric(nombres_error_minimo_por_mapas[[which(niveles_maps == tablero[[5]])]]))-1)
  names(puntuacion_victoria) = NULL
  if(puntuacion_victoria == 1){
    return(1)}
  else{
    return(-1)}
}
backpropagate = function(nodo, puntuacion){
  #Actualizar los nodos hasta la raiz
  while(!is.null(nodo)){
    #actualizar visita
    nodo$visitas = nodo$visitas +1
    #actualizar puntuacion
    nodo$puntuacion = nodo$puntuacion + puntuacion
    #pasar al padre del nodo actual
    nodo = nodo$parent}
}
politica_best_move = function(lista_nodos, turno_actual){
  #Contruir la lista de los posibles estados.
  lista_estados = NULL
  for(nodo in lista_nodos){
    if(turno_actual == 1 || turno_actual == 2 || turno_actual == 8 || turno_actual == 9){
      lista_estados = append(lista_estados, tail(nodo$tablero[[2]], 1))}
    else{
      lista_estados = append(lista_estados, tail(nodo$tablero[[3]], 1))}}
  #creacion de un vector vacio que guardara los pesos del heroe elejido en cada estado
  pesos = NULL
  #Creacion de variable que tendra el peso total de todos los pesos.
  peso_total = 0
  #Pasar por cada heroe posible
  for (estado_actual in lista_estados){
    tmp_peso = data_Keys_Heroes[which(data_Keys_Heroes$primaryname == estado_actual),4]
    pesos = append(pesos, tmp_peso)
    peso_total = peso_total + tmp_peso}
  #elejir un numero aleatorio entre 1 al peso_total
  peso_aleatorio = sample(1:(peso_total), 1)
  #Crear una variable numerica que almacena la direccion del estado a elegir
  direccion = 1
  #Ir por cada peso del vector para ver que jugara se elige
  for (peso_actual in pesos){
    #Si el peso aleatorio es inferior al peso actual
    if(peso_aleatorio <= peso_actual){
      #Retornar estado en la direccion correspondiente
      return(lista_nodos[[direccion]])}
    #Disminuir el peso aleatorio por el peso actual
    peso_aleatorio = peso_aleatorio - peso_actual
    #aumentar la direccion apropiadamente
    direccion = direccion + 1}
  #No se deberia llegar a esta parte
  stop("El peso aleatorio es mayor que el peso total")
}
get_best_move = function(nodo, constante_exploracion, booleano_politica = TRUE){
  #definir mejor puntuacion y mejor movimiento
  tmp_best_score = -Inf
  tmp_best_moves = list()
  #recorrer los hijos:
  for (nodo_hijo in nodo$children){
    #Definir el equipo actualmente jugando
    if(nodo$tablero[[1]] == 1 || nodo$tablero[[1]] == 3 || nodo$tablero[[1]] == 6 || nodo$tablero[[1]] == 7 || nodo$tablero[[1]] == 9 || nodo$tablero[[1]] == 12 || nodo$tablero[[1]] == 13){
      flag_jugador_actual = 1}
    else if(nodo$tablero[[1]] == 2 || nodo$tablero[[1]] == 4 || nodo$tablero[[1]] == 5 || nodo$tablero[[1]] == 8 || nodo$tablero[[1]] == 10 || nodo$tablero[[1]] == 11 || nodo$tablero[[1]] == 14){
      flag_jugador_actual = -1}
    #Obtener la puntiacion de movimiento usando la formula UCT
    tmp_move_score = ((flag_jugador_actual*nodo_hijo$puntuacion)/nodo_hijo$visitas) + (constante_exploracion*(sqrt(log(nodo$visitas)/nodo_hijo$visitas)))
    #Si se encuentra un mejor movimiento, cambiar valor tmp_best_score y lista tmp_best_moves
    if(tmp_move_score > tmp_best_score){
      tmp_best_score = tmp_move_score
      tmp_best_moves = list(nodo_hijo)}
    else if(tmp_move_score == tmp_best_score){
      tmp_best_moves = append(tmp_best_moves, nodo_hijo)}}
  if(booleano_politica){
    #Se devuelve una de las mejores jugadas segun la politica de best move
    return(politica_best_move(tmp_best_moves, nodo$tablero[[1]]))}
  else{
    #Aca se esta devolviendo una de las mejores jugadas al azar
    return(tmp_best_moves[[sample(1:(length(tmp_best_moves)), 1)]])}
}

################################################# UI #################################################

######## FUNCTIONS ########

radioImages = function(inputId, images, values){
  radios = lapply(
    seq_along(images),
    function(i){
      id = paste0(inputId, i)
      tagList(
        tags$input(
          type = "radio",
          name = inputId,
          id = id,
          class = "input-hidden",
          value = as.character(values[i])),
        tags$label(
          `for` = id,
          tags$img(
            src = images[i])))})
  do.call(
    function(...) div(..., class = "shiny-input-radiogroup", id = inputId), 
    radios)
}

######## THEME ########
theme_purple_gradient <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial",
  appFontColor = "rgb(128,177,221)",
  primaryFontColor = "rgb(255,255,255)",
  infoFontColor = "rgb(255,255,255)",
  successFontColor = "rgb(255,255,255)",
  warningFontColor = "rgb(255,255,255)",
  dangerFontColor = "rgb(255,255,255)"
  ,bodyBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(49,56,107)"
    ,colorMiddle = "rgb(71,59,109)"
    ,colorEnd = "rgb(78,88,149)"
    ,colorStartPos = 0
    ,colorMiddlePos = 70
    ,colorEndPos = 100
  )
  
  ### header
  ,logoBackColor = "rgb(49,56,107)"
  
  ,headerButtonBackColor = "rgb(49,56,107)"
  ,headerButtonIconColor = "rgb(62,133,179)"
  ,headerButtonBackColorHover = "rgb(49,56,107)"
  ,headerButtonIconColorHover = "rgb(255,255,255)"
  
  ,headerBackColor = "rgb(49,56,107)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(49,56,107)"
    ,colorMiddle = "rgb(71,59,109)"
    ,colorEnd = "rgb(78,88,149)"
    ,colorStartPos = 0
    ,colorMiddlePos = 70
    ,colorEndPos = 100
  )
  
  ,sidebarShadowRadius = ""
  ,sidebarPadding = 10
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarMenuBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(48,103,157)"
    ,colorMiddle = "rgb(65,79,129)"
    ,colorEnd = "rgb(55,70,120)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarMenuPadding = 5
  ,sidebarMenuBorderRadius = 20
  
  ,sidebarUserTextColor = "rgb(128,177,221)"
  
  ,sidebarSearchBackColor = "rgb(40,70,115)"
  ,sidebarSearchIconColor = "rgb(50,115,145)"
  ,sidebarSearchBorderColor = "rgb(30,60,105)"
  
  ,sidebarTabTextColor = "rgb(128,177,221)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(56,137,189)"
    ,colorMiddle = "rgb(65,95,145)"
    ,colorEnd = "rgb(68,84,137)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "30px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(56,137,189)"
    ,colorMiddle = "rgb(65,95,145)"
    ,colorEnd = "rgb(68,84,137)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "30px"
  
  ### boxes
  ,boxBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(70,75,125)"
    ,colorMiddle = "rgb(65,79,129)"
    ,colorEnd = "rgb(55,70,120)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,boxBorderRadius = 15
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(49,56,107)"
  ,boxPrimaryColor = "rgb(141,192,241)"
  ,boxInfoColor = "rgb(20,100,160)"
  ,boxSuccessColor = "rgb(64,186,170)"
  ,boxWarningColor = "rgb(255,217,144)"
  ,boxDangerColor = "rgb(249,144,144)"
  
  ,tabBoxTabColor = "rgb(80,95,155)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(128,177,221)"
  ,tabBoxTabTextColorSelected = "rgb(255,255,255)"
  ,tabBoxBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(70,75,125)"
    ,colorMiddle = "rgb(65,79,129)"
    ,colorEnd = "rgb(55,70,120)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,tabBoxHighlightColor = "rgb(80,95,155)"
  ,tabBoxBorderRadius = 15
  
  ### inputs
  ,buttonBackColor = "rgb(72,190,229)"
  ,buttonTextColor = "rgb(40,63,106)"
  ,buttonBorderColor = "rgb(72,190,229)"
  ,buttonBorderRadius = 20
  
  ,buttonBackColorHover = "rgb(115,210,240)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(115,210,240)"
  
  ,textboxBackColor = "rgb(40,70,115)"
  ,textboxBorderColor = "rgb(30,60,105)"
  ,textboxBorderRadius = 20
  ,textboxBackColorSelect = "rgb(40,70,115)"
  ,textboxBorderColorSelect = "rgb(30,60,105)"
  
  ### tables
  ,tableBackColor = "transparent"
  ,tableBorderColor = "rgb(80,95,155)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1)

######## HEAD ########
header = dashboardHeader(
  title = "SISHOST", 
  titleWidth = 310
)

####### SIDEBAR ######
sidebar = dashboardSidebar(
  width = 310,
  tags$br(),
  sidebarMenu(
    menuItem("Detalles de la partida", tabName = "preparacion")),
  sliderInput(inputId = "map", label = "Selecciona un mapa", min = 1, max = 22, value = 1),
  tags$hr(style="border-color: rgb(40,70,115);"),
  tags$div(class = "customUsuarios", 
           selectInput(inputId = "usuarios1", label = "Selecciona participante 1",
                       choices = list(
                         "Usuario" = 1, 
                         "MCTS" = 2,
                         "Random" = 3,
                         "Best_Winrate" = 4))),
  tags$h4("VS", style = "text-align: center;"),
  tags$div(class = "customUsuarios", 
           selectInput(inputId = "usuarios2", label = "Selecciona participante 2", selected = 2,
                       choices = list(
                         "Usuario" = 1, 
                         "MCTS" = 2,
                         "Random" = 3,
                         "Best_Winrate" = 4))),
  tags$hr(style="border-color: rgb(40,70,115);"),
  sidebarMenu(
    menuItem("Estado de la partida", tabName = "seleccion"))
)

######## BODY ########
body = dashboardBody(
  tags$head(tags$style(HTML('
      .customUsuarios .selectize-dropdown-content > .option {
        color: rgb(128,177,221);}
      .skin-purple .sidebar-menu > li.active > a{
        border-left-color: rgb(128,177,221);
        background-color: rgb(128,177,221);
        border-radius: 15px}
      .skin-purple .sidebar-menu > li:hover > a {
        border-left-color: rgb(128,177,221);
        background-color: rgb(128,177,221);
        border-radius: 15px}
      .skin-purple .main-sidebar .sidebar .sidebar-menu a{
        text-align: center;
        padding-right: 25px;}
      .input-hidden {
        position: absolute;
        left: -9999px;}
      input[type=radio] + label>img {
        width: 60px;
        height: 60px;
        transition: 500ms all;}
      input[type=radio]:checked + label>img {
        border: 1px solid rgb(128,177,221);
        box-shadow: 0 0 3px 3px rgb(128,177,221);}'))),
  shinyjs::useShinyjs(),
  theme_purple_gradient,
  setBackgroundImage(
    src = "Fondo.jpg",
    shinydashboard = TRUE),
  tabItems(
    tabItem(tabName = "preparacion", 
            fluidRow(
              column(6, 
                     box(
                       width = 12,
                       title = textOutput("nombreMap"),
                       imageOutput("imgMap", inline = TRUE))),
              column(6, 
                     box(width = 12))),
            fluidRow(
              box(width = 12,
                  column(5, 
                         tags$h3("Primera seleccion", style = "text-align: center;"),
                         tags$div(imageOutput("imgUser1", inline = TRUE), style = "text-align: center;"),
                         tags$h5(textOutput("nombreUser1"), style = "text-align: center;")),
                  column(2,
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$h1("VS", style = "text-align: center; font-size: 60px; font-family: Times New Roman;"),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br()),
                  column(5, 
                         tags$h3("Segunda seleccion", style = "text-align: center;"),
                         tags$div(imageOutput("imgUser2", inline = TRUE), style = "text-align: center;"),
                         tags$h5(textOutput("nombreUser2"), style = "text-align: center;"))))),
    tabItem(tabName = "seleccion",
            fluidRow(
              actionButton("botonInicio", "Comenzar partida", style = "font-size: 24px; color: white; display: block; margin-left: auto; margin-right: auto; width: 250px;"),
              shinyjs::hidden(p(id = "mensajeInicioPartida", "Partida iniciada...")),
              shinyjs::hidden(p(id = "mensajeSeleccion", "Selecciona un heroe y confirma...")),
              textOutput("resultadoPartida"),
              tags$br()),
            fluidRow(box(width = 12, tags$h1("Fase de seleccion", style = "text-align: center;"))),
            fluidRow(
              column(2,
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion1Equipo1", inline = TRUE), style = "text-align: center;"))),
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion2Equipo1", inline = TRUE), style = "text-align: center;"))),
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion3Equipo1", inline = TRUE), style = "text-align: center;"))),
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion4Equipo1", inline = TRUE), style = "text-align: center;"))),
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion5Equipo1", inline = TRUE), style = "text-align: center;")))),
              column(8,
                     fluidRow(
                       box(width = 12, height = 400,
                           tags$br(),
                           box(width = 12, style = "background-color: black; border: 15px solid black; height: 350px; overflow-y: scroll; overflow-x: hidden;", solidHeader = TRUE,
                               fluidRow(radioImages("heroesRadio", 
                                                    images = c("/Heroes/Bruiser/Artanis.png", "/Heroes/Bruiser/Chen.png", "/Heroes/Bruiser/D.Va.png", "/Heroes/Bruiser/Deathwing.png", "/Heroes/Bruiser/Dehaka.png", "/Heroes/Bruiser/Gazlowe.png", "/Heroes/Bruiser/Hogger.png", "/Heroes/Bruiser/Imperius.png", "/Heroes/Bruiser/Leoric.png", "/Heroes/Bruiser/Malthael.png", "/Heroes/Bruiser/Ragnaros.png", "/Heroes/Bruiser/Rexxar.png", "/Heroes/Bruiser/Sonya.png", "/Heroes/Bruiser/Thrall.png", "/Heroes/Bruiser/Varian.png", "/Heroes/Bruiser/Xul.png", "/Heroes/Bruiser/Yrel.png",
                                                               "/Heroes/Healer/Alexstrasza.png", "/Heroes/Healer/Ana.png", "/Heroes/Healer/Anduin.png", "/Heroes/Healer/Auriel.png", "/Heroes/Healer/Brightwing.png", "/Heroes/Healer/Deckard.png", "/Heroes/Healer/Kharazim.png", "/Heroes/Healer/LiLi.png", "/Heroes/Healer/Lt.Morales.png", "/Heroes/Healer/Lucio.png", "/Heroes/Healer/Malfurion.png", "/Heroes/Healer/Rehgar.png", "/Heroes/Healer/Stukov.png", "/Heroes/Healer/Tyrande.png", "/Heroes/Healer/Uther.png", "/Heroes/Healer/Whitemane.png",
                                                               "/Heroes/MeleeAssassin/Alarak.png", "/Heroes/MeleeAssassin/Illidan.png", "/Heroes/MeleeAssassin/Kerrigan.png", "/Heroes/MeleeAssassin/Maiev.png", "/Heroes/MeleeAssassin/Murky.png", "/Heroes/MeleeAssassin/Qhira.png", "/Heroes/MeleeAssassin/Samuro.png", "/Heroes/MeleeAssassin/TheButcher.png", "/Heroes/MeleeAssassin/Valeera.png", "/Heroes/MeleeAssassin/Zeratul.png",
                                                               "/Heroes/RangedAssassin/Azmodan.png", "/Heroes/RangedAssassin/Cassia.png", "/Heroes/RangedAssassin/Chromie.png", "/Heroes/RangedAssassin/Falstad.png", "/Heroes/RangedAssassin/Fenix.png", "/Heroes/RangedAssassin/Gall.png", "/Heroes/RangedAssassin/Genji.png", "/Heroes/RangedAssassin/Greymane.png", "/Heroes/RangedAssassin/Guldan.png", "/Heroes/RangedAssassin/Hanzo.png", "/Heroes/RangedAssassin/Jaina.png", "/Heroes/RangedAssassin/Junkrat.png", "/Heroes/RangedAssassin/Kaelthas.png", "/Heroes/RangedAssassin/KelThuzad.png", "/Heroes/RangedAssassin/LiMing.png", "/Heroes/RangedAssassin/Lunara.png", "/Heroes/RangedAssassin/Mephisto.png", "/Heroes/RangedAssassin/Nazeebo.png", "/Heroes/RangedAssassin/Nova.png", "/Heroes/RangedAssassin/Orphea.png", "/Heroes/RangedAssassin/Probius.png", "/Heroes/RangedAssassin/Raynor.png", "/Heroes/RangedAssassin/Sgt.Hammer.png", "/Heroes/RangedAssassin/Sylvanas.png", "/Heroes/RangedAssassin/Tassadar.png", "/Heroes/RangedAssassin/Tracer.png", "/Heroes/RangedAssassin/Tychus.png", "/Heroes/RangedAssassin/Valla.png", "/Heroes/RangedAssassin/Zagara.png", "/Heroes/RangedAssassin/Zuljin.png",
                                                               "/Heroes/Support/Abathur.png", "/Heroes/Support/Medivh.png", "/Heroes/Support/TheLostVikings.png", "/Heroes/Support/Zarya.png",
                                                               "/Heroes/Tank/Anubarak.png", "/Heroes/Tank/Arthas.png", "/Heroes/Tank/Blaze.png", "/Heroes/Tank/Cho.png", "/Heroes/Tank/Diablo.png", "/Heroes/Tank/E.T.C..png", "/Heroes/Tank/Garrosh.png", "/Heroes/Tank/Johanna.png", "/Heroes/Tank/MalGanis.png", "/Heroes/Tank/Mei.png", "/Heroes/Tank/Muradin.png", "/Heroes/Tank/Stitches.png", "/Heroes/Tank/Tyrael.png"), 
                                                    values = c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel",
                                                               "Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane",
                                                               "Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul",
                                                               "Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin",
                                                               "Abathur", "Medivh", "TheLostVikings", "Zarya",
                                                               "Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")))))),
                     fluidRow(
                       shinyjs::disabled(actionButton("botonConfirmarSeleccion", "Confirmar seleccion", style = "font-size: 16px; color: white; display: block; margin-left: auto; margin-right: auto; width: 250px;")),
                       tags$br())),
              column(2,
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion1Equipo2", inline = TRUE), style = "text-align: center;"))),
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion2Equipo2", inline = TRUE), style = "text-align: center;"))),
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion3Equipo2", inline = TRUE), style = "text-align: center;"))),
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion4Equipo2", inline = TRUE), style = "text-align: center;"))),
                     fluidRow(
                       box(width = 12, height = "80px",
                           tags$div(imageOutput("seleccion5Equipo2", inline = TRUE), style = "text-align: center;"))))),
            fluidRow(box(width = 12, 
                         tags$h1("Baneos", style = "text-align: center;"),
                         tags$br(),
                         column(3, tags$div(style = "text-align: center;", imageOutput("baneo1", inline = TRUE))),
                         column(3, tags$div(style = "text-align: center;", imageOutput("baneo2", inline = TRUE))),
                         column(3, tags$div(style = "text-align: center;", imageOutput("baneo3", inline = TRUE))),
                         column(3, tags$div(style = "text-align: center;", imageOutput("baneo4", inline = TRUE)))))))
)

ui = dashboardPage(header, sidebar, body, skin = "purple")


##################################################### SERVER #####################################################


server = function(input, output, session)
{
  output$nombreMap = renderText({
    switch(input$map, 
           "Battlefield of Eternity", 
           "Blackhearts Bay", 
           "Cursed Hollow",
           "Dragon Shire",
           "Garden of Terror",
           "Haunted Mines",
           "Infernal Shrines",
           "Sky Temple",
           "Tomb of the Spider Queen",
           "Towers of Doom",
           "Lost Cavern",
           "Braxis Holdout",
           "Warhead Junction",
           "Silver City",
           "Braxis Outpost",
           "Hanamura Temple",
           "Checkpoint: Hanamura",
           "Pull Party",
           "Volskaya Foundry",
           "Industrial District",
           "Escape From Braxis",
           "Alterac Pass")})
  
  output$imgMap = renderImage({
    switch(input$map, 
           list(src = file.path("www", "battlefield_of_eternity.jpg"), width = "100%"), 
           list(src = file.path("www", "Blackhearts_Bay.jpg"), width = "100%"), 
           list(src = file.path("www", "Cursed_Hollow.jpg"), width = "100%"),
           list(src = file.path("www", "Dragon-shire.jpg"), width = "100%"),
           list(src = file.path("www", "Garden_of_Terror.jpg"), width = "100%"),
           list(src = file.path("www", "Haunted_Mines.jpg"), width = "100%"),
           list(src = file.path("www", "Infernal_Shrines.jpg"), width = "100%"),
           list(src = file.path("www", "Sky_Temple.jpg"), width = "100%"),
           list(src = file.path("www", "Tomb_of_the_Spider_Queen.jpg"), width = "100%"),
           list(src = file.path("www", "Towers_of_Doom.jpg"), width = "100%"),
           list(src = file.path("www", "Lost_Cavern.jpg"), width = "100%"),
           list(src = file.path("www", "Braxis_Holdout.jpg"), width = "100%"),
           list(src = file.path("www", "Warhead_Junction.jpg"), width = "100%"),
           list(src = file.path("www", "Silver_City.jpg"), width = "100%"),
           list(src = file.path("www", "Braxis_Outpost.jpg"), width = "100%"),
           list(src = file.path("www", "Hanamura_Temple.jpg"), width = "100%"),
           list(src = file.path("www", "Checkpoint_Hanamura.jpg"), width = "100%"),
           list(src = file.path("www", "Pull_Party.jpg"), width = "100%"),
           list(src = file.path("www", "Volskaya_Foundry.jpg"), width = "100%"),
           list(src = file.path("www", "Industrial_District.jpg"), width = "100%"),
           list(src = file.path("www", "Escape_From_Braxis.jpg"), width = "100%"),
           list(src = file.path("www", "Alterac_Pass.jpg"), width = "100%"))}, 
    deleteFile = FALSE)
  
  output$imgUser1 = renderImage({
    switch(as.numeric(input$usuarios1), 
           list(src = file.path("www", "Icon.png"), heigth = "200", width = "200"), 
           list(src = file.path("www", "CPU.png"), heigth = "200", width = "200"), 
           list(src = file.path("www", "CPU.png"), heigth = "200", width = "200"),
           list(src = file.path("www", "CPU.png"), heigth = "200", width = "200"))},
    deleteFile = FALSE)
  
  output$nombreUser1 = renderText({
    switch(as.numeric(input$usuarios1), 
           "Usuario", 
           "Monte Carlo tree search", 
           "Seleccion aleatoria",
           "Seleccion mejor winrate")})
  
  output$imgUser2 = renderImage({
    switch(as.numeric(input$usuarios2), 
           list(src = file.path("www", "Icon.png"), heigth = "200", width = "200"), 
           list(src = file.path("www", "CPU.png"), heigth = "200", width = "200"), 
           list(src = file.path("www", "CPU.png"), heigth = "200", width = "200"),
           list(src = file.path("www", "CPU.png"), heigth = "200", width = "200"))},
    deleteFile = FALSE)
  
  output$nombreUser2 = renderText({
    switch(as.numeric(input$usuarios2), 
           "Usuario", 
           "Monte Carlo tree search", 
           "Seleccion aleatoria",
           "Seleccion mejor winrate")})
  
  valoresReactivos = reactiveValues(
    isPartidaTerminada = FALSE,
    turno = 0,
    nodo = "crear nodo",
    banderaUsuario = 0,
    banderaMCTS = 0,
    banderaRandom = 0,
    banderaWinrate = 0,
    seleccion1Equipo1 = "randomPJ",
    seleccion2Equipo1 = "randomPJ",
    seleccion3Equipo1 = "randomPJ",
    seleccion4Equipo1 = "randomPJ",
    seleccion5Equipo1 = "randomPJ",
    seleccion1Equipo2 = "randomPJ",
    seleccion2Equipo2 = "randomPJ",
    seleccion3Equipo2 = "randomPJ",
    seleccion4Equipo2 = "randomPJ",
    seleccion5Equipo2 = "randomPJ",
    baneo1 = "randomPJ",
    baneo2 = "randomPJ",
    baneo3 = "randomPJ",
    baneo4 = "randomPJ")
  
  output$seleccion1Equipo1 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion1Equipo1
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$seleccion2Equipo1 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion2Equipo1
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$seleccion3Equipo1 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion3Equipo1
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$seleccion4Equipo1 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion4Equipo1
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$seleccion5Equipo1 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion5Equipo1
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$seleccion1Equipo2 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion1Equipo2
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$seleccion2Equipo2 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion2Equipo2
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$seleccion3Equipo2 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion3Equipo2
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$seleccion4Equipo2 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion4Equipo2
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$seleccion5Equipo2 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$seleccion5Equipo2
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$baneo1 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$baneo1
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$baneo2 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$baneo2
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$baneo3 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$baneo3
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  output$baneo4 = renderImage({
    nombreCarpeta = "Random"
    heroeSeleccionado = valoresReactivos$baneo4
    if(heroeSeleccionado %in% c("Artanis", "Chen", "D.Va", "Deathwing", "Dehaka", "Gazlowe", "Hogger", "Imperius", "Leoric", "Malthael", "Ragnaros", "Rexxar", "Sonya", "Thrall", "Varian", "Xul", "Yrel")){
      nombreCarpeta = "Bruiser"}
    else if(heroeSeleccionado %in% c("Alexstrasza", "Ana", "Anduin", "Auriel", "Brightwing", "Deckard", "Kharazim", "LiLi", "Lt.Morales", "Lucio", "Malfurion", "Rehgar", "Stukov", "Tyrande", "Uther", "Whitemane")){
      nombreCarpeta = "Healer"}
    else if(heroeSeleccionado %in% c("Alarak", "Illidan", "Kerrigan", "Maiev", "Murky", "Qhira", "Samuro", "TheButcher", "Valeera", "Zeratul")){
      nombreCarpeta = "MeleeAssassin"}
    else if(heroeSeleccionado %in% c("Azmodan", "Cassia", "Chromie", "Falstad", "Fenix", "Gall", "Genji", "Greymane", "Guldan", "Hanzo", "Jaina", "Junkrat", "Kaelthas", "KelThuzad", "LiMing", "Lunara", "Mephisto", "Nazeebo", "Nova", "Orphea", "Probius", "Raynor", "Sgt.Hammer", "Sylvanas", "Tassadar", "Tracer", "Tychus", "Valla", "Zagara", "Zuljin")){
      nombreCarpeta = "RangedAssassin"}
    else if(heroeSeleccionado %in% c("Abathur", "Medivh", "TheLostVikings", "Zarya")){
      nombreCarpeta = "Support"}
    else if(heroeSeleccionado %in% c("Anubarak", "Arthas", "Blaze", "Cho", "Diablo", "E.T.C.", "Garrosh", "Johanna", "MalGanis", "Mei", "Muradin", "Stitches", "Tyrael")){
      nombreCarpeta = "Tank"}
    list(src = file.path("www", "Heroes", nombreCarpeta, paste0(heroeSeleccionado, ".png")), heigth = "60", width = "60")},
    deleteFile = FALSE)
  
  observeEvent(input$botonInicio, {
    shinyjs::disable("botonInicio")
    shinyjs::disable("map")
    shinyjs::disable("usuarios1")
    shinyjs::disable("usuarios2")
    shinyjs::show("mensajeInicioPartida")
    valoresReactivos$baneo1 = "randomPJ"
    valoresReactivos$baneo2 = "randomPJ"
    valoresReactivos$seleccion1Equipo1 = "randomPJ"
    valoresReactivos$seleccion1Equipo2 = "randomPJ"
    valoresReactivos$seleccion2Equipo2 = "randomPJ"
    valoresReactivos$seleccion2Equipo1 = "randomPJ"
    valoresReactivos$seleccion3Equipo1 = "randomPJ"
    valoresReactivos$baneo3 = "randomPJ"
    valoresReactivos$baneo4 = "randomPJ"
    valoresReactivos$seleccion3Equipo2 = "randomPJ"
    valoresReactivos$seleccion4Equipo2 = "randomPJ"
    valoresReactivos$seleccion4Equipo1 = "randomPJ"
    valoresReactivos$seleccion5Equipo1 = "randomPJ"
    valoresReactivos$seleccion5Equipo2 = "randomPJ"
    valoresReactivos$isPartidaTerminada = FALSE
    #Inicio de la partida
    valoresReactivos$turno = 1})
  
  observeEvent(valoresReactivos$turno, {
    switch(valoresReactivos$turno,
           switch(as.numeric(input$usuarios1), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios2), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios1), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios2), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios2), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios1), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios1), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios2), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios1), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios2), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios2), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios1), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios1), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           switch(as.numeric(input$usuarios2), 
                  {valoresReactivos$banderaUsuario = valoresReactivos$banderaUsuario + 1}, 
                  {valoresReactivos$banderaMCTS = valoresReactivos$banderaMCTS + 1}, 
                  {valoresReactivos$banderaRandom = valoresReactivos$banderaRandom + 1},
                  {valoresReactivos$banderaWinrate = valoresReactivos$banderaWinrate + 1}),
           {valoresReactivos$isPartidaTerminada = TRUE})})
  
  observeEvent(valoresReactivos$banderaUsuario, {
    if(valoresReactivos$banderaUsuario > 0){
      print("Usuario")
      shinyjs::enable("mensajeSeleccion")
      shinyjs::enable("botonConfirmarSeleccion")}})
  
  observeEvent(input$botonConfirmarSeleccion, {
    if(!is.null(input$heroesRadio)){ #               *Se debe de agregar el hecho de que no debe de aceptar heroes ya escogidos*
      heroeEscogido = input$heroesRadio
      switch(valoresReactivos$turno,
             {valoresReactivos$baneo1 = heroeEscogido},
             {valoresReactivos$baneo2 = heroeEscogido},
             {valoresReactivos$seleccion1Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion1Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion2Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion2Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion3Equipo1 = heroeEscogido},
             {valoresReactivos$baneo3 = heroeEscogido},
             {valoresReactivos$baneo4 = heroeEscogido},
             {valoresReactivos$seleccion3Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion4Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion4Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion5Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion5Equipo2 = heroeEscogido})
      #        *Realizar jugada en nodo*
      shinyjs::disable("botonConfirmarSeleccion")
      shinyjs::disable("mensajeSeleccion")
      valoresReactivos$turno = valoresReactivos$turno + 1}})
  
  observeEvent(valoresReactivos$banderaMCTS, {
    if(valoresReactivos$banderaMCTS > 0){
      print("Monte Carlo tree search")
      heroeEscogido = "randomPJ"
      switch(valoresReactivos$turno,
             {valoresReactivos$baneo1 = heroeEscogido},
             {valoresReactivos$baneo2 = heroeEscogido},
             {valoresReactivos$seleccion1Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion1Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion2Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion2Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion3Equipo1 = heroeEscogido},
             {valoresReactivos$baneo3 = heroeEscogido},
             {valoresReactivos$baneo4 = heroeEscogido},
             {valoresReactivos$seleccion3Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion4Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion4Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion5Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion5Equipo2 = heroeEscogido})
      valoresReactivos$turno = valoresReactivos$turno + 1}})
  
  observeEvent(valoresReactivos$banderaRandom, {
    if(valoresReactivos$banderaRandom > 0){
      print("Seleccion aleatoria")
      heroeEscogido = "randomPJ"
      switch(valoresReactivos$turno,
             {valoresReactivos$baneo1 = heroeEscogido},
             {valoresReactivos$baneo2 = heroeEscogido},
             {valoresReactivos$seleccion1Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion1Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion2Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion2Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion3Equipo1 = heroeEscogido},
             {valoresReactivos$baneo3 = heroeEscogido},
             {valoresReactivos$baneo4 = heroeEscogido},
             {valoresReactivos$seleccion3Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion4Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion4Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion5Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion5Equipo2 = heroeEscogido})
      valoresReactivos$turno = valoresReactivos$turno + 1}})
  
  observeEvent(valoresReactivos$banderaWinrate, {
    if(valoresReactivos$banderaWinrate > 0){
      print("Seleccion mejor winrate")
      heroeEscogido = "randomPJ"
      switch(valoresReactivos$turno,
             {valoresReactivos$baneo1 = heroeEscogido},
             {valoresReactivos$baneo2 = heroeEscogido},
             {valoresReactivos$seleccion1Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion1Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion2Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion2Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion3Equipo1 = heroeEscogido},
             {valoresReactivos$baneo3 = heroeEscogido},
             {valoresReactivos$baneo4 = heroeEscogido},
             {valoresReactivos$seleccion3Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion4Equipo2 = heroeEscogido},
             {valoresReactivos$seleccion4Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion5Equipo1 = heroeEscogido},
             {valoresReactivos$seleccion5Equipo2 = heroeEscogido})
      valoresReactivos$turno = valoresReactivos$turno + 1}})
  
  output$resultadoPartida = renderText({
    if(valoresReactivos$isPartidaTerminada){
      shinyjs::enable("botonInicio")
      shinyjs::enable("map")
      shinyjs::enable("usuarios1")
      shinyjs::enable("usuarios2")
      shinyjs::hide("mensajeInicioPartida")
      "Termino"}
  })
}

######################################################### SHINY OBJECT ##########################################################

shinyApp(ui = ui, server = server)