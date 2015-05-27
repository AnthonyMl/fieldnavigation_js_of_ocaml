open Vec2


let pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211
let gridSize = 20.
let startPosition = (-. gridSize, -. gridSize)
let robotDisplayRadius = 0.3
let robotMaxSpeed = 11.



let time () = (jsnew Js.date_now () ) ## getTime ()


let resize ctx _ _ =
  let width  = Js.Optdef.get (Dom_html.window ## innerWidth ) (fun _ -> 1) in
  let height = Js.Optdef.get (Dom_html.window ## innerHeight) (fun _ -> 1) in
  ctx ## canvas ## width  <- width;
  ctx ## canvas ## height <- height;
  ctx ## translate (float_of_int @@ width / 2, float_of_int @@ height / 2);
  ctx ## scale (15., -15.);
  Lwt.return_unit


let forceObstacle obstacle position = match obstacle with

  | Model.Circle circle ->
    let difference = sub position circle.Model.position in
    scale ( (circle.Model.radius ** 2.) /. (length difference ** 3.) ) difference

  | Model.Segment segment ->
    let ab = sub segment.Model.b segment.Model.a in
    let ap = sub position        segment.Model.a in
    let bp = sub position        segment.Model.b in
    if      dot ab ap < 0. then scale ( (segment.Model.width ** 2.) /. (length ap ** 3.) ) ap
    else if dot ab bp > 0. then scale ( (segment.Model.width ** 2.) /. (length bp ** 3.) ) bp
    else
      let perp = normalize (-. snd ab, fst ab) in
      scale (-. (segment.Model.width ** 2.) /. dot perp ap ** 2.) perp


let forceDirection model position =
  let fG = normalize @@ sub model.Model.goal.position position in
  let fO = List.fold_left (fun fAcc obstacle -> add fAcc @@ forceObstacle obstacle position) (0., 0.) model.Model.obstacles in
  normalize @@ add fG fO


let updateRobot dt model =
  if   model.Model.goal.radius > length @@ sub model.Model.robotPosition model.Model.goal.position
  then startPosition
  else add model.Model.robotPosition @@ scale (dt *. robotMaxSpeed) (forceDirection model model.Model.robotPosition)


let updateGoal dt model =
  let x = fst @@ add model.Model.goal.position @@ scale dt model.Model.goal.velocity in
  let x, newVelocity =
    if      x < 0.       then (               -. x, scale (-1.) model.Model.goal.velocity)
    else if x > gridSize then (2. *. gridSize -. x, scale (-1.) model.Model.goal.velocity)
    else                      (                  x,             model.Model.goal.velocity)
  in
  { model.Model.goal with
    position = (x, snd model.Model.goal.position);
    velocity = newVelocity;
  }


let updateObstacle dt obstacle = match obstacle with

  | Model.Circle circle ->
    let x = fst @@ add circle.Model.position @@ scale dt circle.Model.velocity in
    let x, newVelocity =
      if      x < -. gridSize then (-2. *. gridSize -. x, scale (-1.) circle.Model.velocity)
      else if x >    gridSize then ( 2. *. gridSize -. x, scale (-1.) circle.Model.velocity)
      else                         (                   x,             circle.Model.velocity)
    in
    Model.Circle { circle with
      position = (x, snd circle.Model.position);
      velocity = newVelocity;
    }

  | Model.Segment segment -> Model.Segment segment


let update oldTime model =
  let newTime = time () in
  let dt = 0.001 *. (newTime -. oldTime) in
  (newTime, { model with Model.
    robotPosition = updateRobot dt model;
    goal          = updateGoal  dt model;
    obstacles     = List.map (updateObstacle dt) model.Model.obstacles
  })


let drawObstacle ctx obstacle =
  match obstacle with
  | Model.Circle circle ->
    ctx ## beginPath ();
    ctx ## arc (fst circle.Model.position, snd circle.Model.position, circle.Model.radius, 0., 2. *. pi, Js._true);
    ctx ## fill ()

  | Model.Segment segment ->
    ctx ## lineWidth <- 2. *. (segment.Model.width -. robotDisplayRadius);
    ctx ## lineCap   <- Js.string "round";
    ctx ## beginPath ();
    ctx ## moveTo (fst segment.Model.a, snd segment.Model.a);
    ctx ## lineTo (fst segment.Model.b, snd segment.Model.b);
    ctx ## stroke ()


let drawGrid ctx model =
  ctx ## strokeStyle <- Js.string "rgb(200, 200, 200)";
  ctx ## lineCap     <- Js.string "blunt";
  ctx ## lineWidth   <- 0.07;
  for x = - int_of_float gridSize to int_of_float gridSize do
    for y = - int_of_float gridSize to int_of_float gridSize do
      let pos = (float_of_int x, float_of_int y) in
      let cap = add pos @@ scale 0.8 @@ forceDirection model pos in
      ctx ## beginPath ();
      ctx ## moveTo (fst pos, snd pos);
      ctx ## lineTo (fst cap, snd cap);
      ctx ## stroke ()
    done
  done


let draw ctx model =
  ctx ## fillStyle <- Js.string "rgb(64, 128, 255)";
  ctx ## fillRect (-1.5 *. gridSize, -1.5 *. gridSize, 3. *. gridSize, 3. *. gridSize);

  let obstacleStyle = Js.string "rgb(170, 40, 40)" in
  ctx ## fillStyle   <- obstacleStyle;
  ctx ## strokeStyle <- obstacleStyle;
  List.iter (drawObstacle ctx) model.Model.obstacles;

  drawGrid ctx model;

  ctx ## beginPath ();
  ctx ## fillStyle <- Js.string "rgb(200, 200, 40)";
  ctx ## arc (fst model.Model.robotPosition, snd model.Model.robotPosition, robotDisplayRadius, 0., 2. *. pi, Js._true);
  ctx ## fill ();

  ctx ## beginPath ();
  ctx ## fillStyle <- Js.string "rgb(40, 200, 40)";
  ctx ## arc (fst model.Model.goal.position, snd model.Model.goal.position, model.Model.goal.radius, 0., 2. *. pi, Js._true);
  ctx ## fill ()


let rec frame ctx oldTime model =
  let newTime, model = update oldTime model in
  draw ctx model;
  Dom_html._requestAnimationFrame @@ Js.wrap_callback @@ (fun () -> frame ctx newTime model)


let start _ =
  let model = { Model.
    robotPosition = startPosition;
    goal          = { Model.
      position    = (gridSize, gridSize);
      velocity    = (-3., 0.);
      radius      =  1.;
    };
    obstacles     = [ Model.
      Circle  { Model.position = (-12., 13.); velocity = (7., 0.)  ; radius = 4. };
      Circle  { Model.position = (-12., -8.); velocity = (4., 0.)  ; radius = 6. };
      Circle  { Model.position = (2., 3.5)  ; velocity = (-8., 0.) ; radius = 5. };
      Segment { Model       .a = (-7., 4.)  ;        b = (1., -11.); width  = 1.5}
    ]
  } in
  let canvas = Dom_html.createCanvas (Dom_html.window ## document) in
  canvas ## style ## position <- Js.string "absolute";
  canvas ## style ## top      <- Js.string "0px";
  canvas ## style ## left     <- Js.string "0px";
  Dom.appendChild Dom_html.window ## document ## body canvas;
  let ctx = canvas ## getContext (Dom_html._2d_) in
  Lwt.ignore_result @@ Lwt_js_events.onresizes (resize ctx);
  Lwt.ignore_result @@ resize ctx () ();
  frame ctx (time ()) model;
  Js._true


let _ = Dom_html.window ## onload <- Dom_html.handler start
