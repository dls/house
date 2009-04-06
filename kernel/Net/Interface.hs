module Net.Interface where


data Interface m i o
  = Interface { rx :: m i, tx :: o -> m () }

data TimedInterface m i o
  = TimedInterface { rxT :: Maybe Int -> m (Maybe i), txT :: o -> m () }
