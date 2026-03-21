(:mallet-preset :strict
  (:extends :default)
  (:enable :line-length :max 80))

(:mallet-preset :relaxed
  (:extends :none)
  (:enable :trailing-whitespace))

(:mallet-config (:extends :strict))
