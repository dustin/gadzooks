angular.module('hooker', []).
    config(['$routeProvider', '$locationProvider',
            function($routeProvider, $locationProvider) {
                $routeProvider.
                    when('/app/dashboard/', {templateUrl: '/static/partials/dashboard.html',
                                     controller: 'DashboardCtrl'}).
                    otherwise({redirectTo: '/app/dashboard/'});
                $locationProvider.html5Mode(true);
                $locationProvider.hashPrefix('!');
            }]);

function DashboardCtrl($scope, $http) {
    $http.get("/api/currentuser/").success(function(data) {
        $scope.guser = data;
    });

    $http.get("/api/hooks").success(function(data) {
        $scope.hooks = data;
    });

    $scope.repo = $scope.dest = "";
    $scope.hooks = [];

    $scope.nothing = function() {return false;};

    $scope.newHook = function() {
        $http.post("/api/hooks/new",
                   "repo=" + encodeURIComponent($scope.repo) +
                   "&dest=" + encodeURIComponent($scope.dest),
                   {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
            success(function(data) {
                $scope.repo = $scope.dest = "";
                $scope.hooks.push(data);
            });
    };

    $scope.rmTask = function(t) {
        console.log("Removing", t);
        $http.post("/api/hooks/rm",
                   "key=" + encodeURIComponent(t.Key) +
                   "&repo=" + encodeURIComponent(t.repo),
                   {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
            success(function(e) {
                $scope.hooks = _.without($scope.hooks, t);
            });
    };
}
