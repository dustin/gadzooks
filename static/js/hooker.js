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
    $http.get("/api/projects").success(function(data) {
        $scope.projects = data;
    });

    $scope.repo = $scope.dest = "";

    $scope.newname = "";
    $scope.newdeps = [];
    $scope.newhooks = [];

    $scope.newProject = function() {
        console.log("Creating a project.");
        var params = "name=" + $scope.newname;
        for (var i = 0; i < $scope.newdeps.length; i++) {
            params += "&deps=" + encodeURIComponent($scope.newdeps[i]);
        }
        for (var i = 0; i < $scope.newhooks.length; i++) {
            params += "&hooks=" + encodeURIComponent($scope.newhooks[i]);
        }
        $http.post("/api/projects/new", params,
                   {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
            success(function(data) {
                $scope.newname;
                $scope.newdeps = [];
                $scope.newhooks = [];
                $scope.projects.push(data);
            });
    };

    var updateProject = function(p) {
        console.log("Updating", p);
        var params = "name=" + p.name + "&key=" + encodeURIComponent(p.Key);
        for (var i = 0; i < (p.deps || []).length; i++) {
            params += "&deps=" + encodeURIComponent(p.deps[i]);
        }
        for (var i = 0; i < (p.hooks || []).length; i++) {
            params += "&hooks=" + encodeURIComponent(p.hooks[i]);
        }
        $http.post("/api/projects/update", params,
                   {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
            success(function(data) {
                p.newdep = p.newhook = "";
            });
    };

    $scope.rmProjectDep = function(p, d) {
        console.log("Removing", d, "from", p);
        p.deps = _.without(p.deps, d);
        updateProject(p);
    };

    $scope.rmProjectHook = function(p, h) {
        console.log("Removing", h, "from", p);
        p.hooks = _.without(p.hooks, d);
        updateProject(p);
    };

    $scope.addDep = function(p) {
        console.log("Adding", p.newdep, "to", p);
        p.deps = p.deps || [];
        p.deps.push(p.newdep);
        updateProject(p);
    };

    $scope.addHook = function(p) {
        console.log("Adding", p.newhook, "to", p);
        p.hooks = p.hooks || [];
        p.hooks.push(p.newhook);
        updateProject(p);
    };

    $scope.rmProject = function(t) {
        console.log("Removing", t);
        $http.post("/api/projects/rm", "key=" + encodeURIComponent(t.Key),
                   {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
            success(function(e) {
                $scope.projects = _.without($scope.projects, t);
            });
    };

}
