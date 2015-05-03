function PlayWheResultsCtrl($scope, $http) {
  $scope.results = [];

  $scope.getResults = function () {
    $http.jsonp('http://api.playwhesmarter.com/results?callback=JSON_CALLBACK&limit=3')
      .success(function (data) {
        $scope.results = data;
      })
      .error(function (data) {
        /* TODO: Handle error condition */
      });
  }

  $scope.getResults();
}

angular.module('PlayWheSmarter', [])
  .filter('timeOfDay', function () {
    return function (period, as_time) {
      if (as_time) {
        return { 1: '10:30 am', 2: '1:00 pm', 3: '6:30 pm' }[period];
      } else {
        return { 1: 'Morning', 2: 'Midday', 3: 'Evening' }[period];
      }
    };
  })
  .filter('spirit', function () {
    var SPIRITS = [
      {}, /* force 1-based access */
      'centipede',
      'old lady',
      'carriage',
      'dead man',
      'parson man',
      'belly',
      'hog',
      'tiger',
      'cattle',
      'monkey',
      'corbeau',
      'king',
      'crapaud',
      'money',
      'sick woman',
      'jamette',
      'pigeon',
      'water boat',
      'horse',
      'dog',
      'mouth',
      'rat',
      'house',
      'queen',
      'morocoy',
      'fowl',
      'little snake',
      'red fish',
      'opium man',
      'house cat',
      'parson wife',
      'shrimps',
      'spider',
      'blind man',
      'big snake',
      'donkey'
    ];

    return function (number) {
      return SPIRITS[number];
    };
  })
  .filter('prettyDate', function ($window) {
    var MONTHS = [
      '', /* force 1-based access */
      'Jan', 'Feb', 'Mar', 'Apr',
      'May', 'Jun', 'Jul', 'Aug',
      'Sep', 'Oct', 'Nov', 'Dec',
    ];

    return function (date) {
      var parts = date.split('-');

      var year  = parts[0];
      var month = MONTHS[$window.parseInt(parts[1])];
      var day   = $window.parseInt(parts[2]);

      return month + ' ' + day + ', ' + year;
    }
  });
