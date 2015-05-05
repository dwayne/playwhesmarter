app = angular.module 'PlayWheSmarter', []

app.factory 'PlayWheAPI', ['$http', ($http) ->
  {
    getResults: ->
      $http.jsonp('http://api.playwhesmarter.com/results?callback=JSON_CALLBACK&limit=3')
  }
]

app.filter 'timeOfDay', ->
  (period, asTime) ->
    if (asTime)
      { 1: '10:30 am', 2: '1:00 pm', 3: '6:30 pm' }[period]
    else
      { 1: 'Morning', 2: 'Midday', 3: 'Evening' }[period]

app.filter 'spirit', ->
  SPIRITS = [
    {} # force 1-based access
    'centipede'
    'old lady'
    'carriage'
    'dead man'
    'parson man'
    'belly'
    'hog'
    'tiger'
    'cattle'
    'monkey'
    'corbeau'
    'king'
    'crapaud'
    'money'
    'sick woman'
    'jamette'
    'pigeon'
    'water boat'
    'horse'
    'dog'
    'mouth'
    'rat'
    'house'
    'queen'
    'morocoy'
    'fowl'
    'little snake'
    'red fish'
    'opium man'
    'house cat'
    'parson wife'
    'shrimps'
    'spider'
    'blind man'
    'big snake'
    'donkey'
  ]

  (n) -> SPIRITS[n]

app.controller 'PlayWheResultsCtrl', ['$scope', 'PlayWheAPI', ($scope, PlayWheAPI) ->
  $scope.results = []

  PlayWheAPI.getResults()
    .success (data) -> $scope.results = data
]
