'use strict';

var app = angular.module('PlayWheSmarter', []);

app.factory('PlayWheAPI', ['$http', function ($http) {
  return {
    getResults: function getResults() {
      return $http.jsonp('http://api.playwhesmarter.com/results?callback=JSON_CALLBACK&limit=3');
    }
  };
}]);

app.filter('timeOfDay', function () {
  return function (period, asTime) {
    if (asTime) return { 'EM': '10:30 AM', 'AM': '1:00 PM', 'AN': '4:00 PM', 'PM': '6:30 PM' }[period];else return { 'EM': 'Morning', 'AM': 'Midday', 'AN': 'Afternoon', 'PM': 'Evening' }[period];
  };
});

app.filter('spirit', function () {
  var SPIRITS = [{}, // force 1-based access
  'centipede', 'old lady', 'carriage', 'dead man', 'parson man', 'belly', 'hog', 'tiger', 'cattle', 'monkey', 'corbeau', 'king', 'crapaud', 'money', 'sick woman', 'jamette', 'pigeon', 'water boat', 'horse', 'dog', 'mouth', 'rat', 'house', 'queen', 'morocoy', 'fowl', 'little snake', 'red fish', 'opium man', 'house cat', 'parson wife', 'shrimps', 'spider', 'blind man', 'big snake', 'donkey'];

  return function (n) {
    return SPIRITS[n];
  };
});

app.controller('PlayWheResultsCtrl', ['$scope', 'PlayWheAPI', function ($scope, PlayWheAPI) {
  $scope.results = [];

  return PlayWheAPI.getResults().success(function (data) {
    return $scope.results = data;
  });
}]);