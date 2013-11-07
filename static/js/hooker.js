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
    $http.get("/api/groups").success(function(data) {
        $scope.groups = data;
    });

    $scope.repo = $scope.dest = "";

    $scope.newname = "";
    $scope.newgroup = "";
    $scope.newdeps = [];
    $scope.newhooks = [];

    $scope.newProject = function() {
        console.log("Creating a project.");
        var params = "name=" + encodeURIComponent($scope.newname)
            + "&group=" + encodeURIComponent($scope.newgroup);
        for (var i = 0; i < $scope.newdeps.length; i++) {
            params += "&deps=" + encodeURIComponent($scope.newdeps[i]);
        }
        for (var i = 0; i < $scope.newhooks.length; i++) {
            params += "&hooks=" + encodeURIComponent($scope.newhooks[i]);
        }
        $http.post("/api/projects/new", params,
                   {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
            success(function(data) {
                $scope.newname = $scope.newgroup = "";
                $scope.newdeps = [];
                $scope.newhooks = [];
                $scope.projects.push(data);
            });
    };

    var updateProject = function(p) {
        console.log("Updating", p);
        var params = "name=" + p.name + "&key=" + encodeURIComponent(p.Key) +
            "&group=" + encodeURIComponent(p.Group);
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

    $scope.changeGroup = function(p) {
        updateProject(p);
    };

    $scope.rmProjectDep = function(p, d) {
        console.log("Removing", d, "from", p);
        p.deps = _.without(p.deps, d);
        updateProject(p);
    };

    $scope.rmProjectHook = function(p, h) {
        console.log("Removing", h, "from", p);
        p.hooks = _.without(p.hooks, h);
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


function GroupCtrl($scope, $http) {
    $scope.newGroupName = "";
    $scope.newGroup = function() {
        $http.post("/api/groups/new", "name=" + encodeURIComponent($scope.newGroupName),
                   {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
            success(function(e) {
                $scope.newGroupName = "";
                $scope.groups.push(e);
            });
    };


    var updateGroup = function(g) {
        console.log("Updating", g);
        var params = "name=" + g.name + "&key=" + encodeURIComponent(g.Key);
        for (var i = 0; i < (g.members || []).length; i++) {
            params += "&members=" + encodeURIComponent(g.members[i]);
        }
        $http.post("/api/groups/update", params,
                   {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
            success(function(data) {
                g.newMember = "";
            });
    };

    $scope.rmGroupMember = function(g, e) {
        console.log("Removing", e, "from", g);
        g.members = _.without(g.members, e);
        updateGroup(g);
    };

    $scope.addGroupMember = function(g) {
        g.members = g.members || [];
        g.members.push(g.newMember)
        updateGroup(g);
    };

    $scope.rmGroup = function(g) {
        $http.post("/api/groups/rm", "key=" + encodeURIComponent(g.Key),
                   {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
            success(function(data) {
                $scope.groups = _.filter($scope.groups, function(e) {
                    return e.Key != g.Key;
                });
            });
    };
}
