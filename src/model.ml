
type circleData = {
  position : float * float;
  velocity : float * float;
  radius   : float;
}

type segmentData = {
  a     : float * float;
  b     : float * float;
  width : float;
}

type obstacle =
  | Circle  of circleData
  | Segment of segmentData

type model = {
  robotPosition : float * float;
  goal          : circleData;
  obstacles     : obstacle list;
  }
