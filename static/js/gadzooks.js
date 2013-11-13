angular.module('gadzooks', []).
    config(['$routeProvider', '$locationProvider',
            function($routeProvider, $locationProvider) {
                $routeProvider.
                    when('/app/dashboard/', {templateUrl: '/static/partials/dashboard.html',
                                     controller: 'DashboardCtrl'}).
                    otherwise({redirectTo: '/app/dashboard/'});
                $locationProvider.html5Mode(true);
                $locationProvider.hashPrefix('!');
            }]).
    factory('groups', function($http, $q) {
        var rv = {
            list: [],
        };

        rv.add = function(name) {
            var d = $q.defer();

            $http.post("/api/groups/new", "name=" + encodeURIComponent(name),
                       {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
                success(function(data) {
                    d.resolve(data);
                    rv.list.push(data);
                }).error(d.reject);

            return d.promise;
        };

        rv.rm = function(key) {
            var d = $q.defer();

            $http.post("/api/groups/rm", "key=" + encodeURIComponent(key),
                       {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
                success(function(data) {
                    rv.list = _.filter(rv.list, function(e) {
                        return e.Key != key;
                    });
                    d.resolve(data);
                }).error(d.reject);

            return d;
        };

        var updateGroup = function(g) {
            var d = $q.defer();

            var params = "name=" + g.name + "&key=" + encodeURIComponent(g.Key);
            for (var i = 0; i < (g.members || []).length; i++) {
                params += "&members=" + encodeURIComponent(g.members[i]);
            }
            $http.post("/api/groups/update", params,
                       {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
                success(d.resolve).error(d.reject);

            return d.promise;
        };

        rv.addMember = function(g, member) {
            g.members = g.members || [];
            g.members.push(member);
            return updateGroup(g);
        };

        rv.rmMember = function(g, member) {
            g.members = _.without(g.members, member);
            return updateGroup(g);
        };

        $http.get("/api/groups").success(function(data) {
            rv.list = data;
        });

        return rv;
    }).
    factory('projects', function($http, $q) {
        var rv = { list: [] };

        $http.get("/api/projects").success(function(data) {
            rv.list = data;
        });

        rv.add = function(name, group, deps, hooks) {
            var d = $q.defer();
            var params = "name=" + encodeURIComponent(name)
                + "&group=" + encodeURIComponent(group);
            for (var i = 0; i < deps.length; i++) {
                params += "&deps=" + encodeURIComponent(deps[i]);
            }
            for (var i = 0; i < hooks.length; i++) {
                params += "&hooks=" + encodeURIComponent(hooks[i]);
            }
            $http.post("/api/projects/new", params,
                       {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
                success(function(data) {
                    rv.list.push(data);
                    d.resolve(data);
                }).error(d.reject);

            return d.promise;
        };

        rv.rm = function(p) {
            $http.post("/api/projects/rm", "key=" + encodeURIComponent(p.Key),
                       {headers: {"Content-Type": "application/x-www-form-urlencoded"}}).
                success(function(e) {
                    rv.list = _.without(rv.list, p);
                });
        };

        rv.update = function(p) {
            var d = $q.defer();

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
                success(d.resolve).error(d.reject);

            return d.promise;
        };

        rv.rmDep = function(p, dep) {
            p.deps = _.without(p.deps, dep);
            return rv.update(p);
        };

        rv.addDep = function(p, dep) {
            p.deps = p.deps || [];
            p.deps.push(dep);
            return rv.update(p);
        };

        rv.rmHook = function(p, hook) {
            p.hooks = _.without(p.hooks, hook);
            return rv.update(p);
        };

        rv.addHook = function(p, hook) {
            p.hooks = p.hooks || [];
            p.hooks.push(hook);
            return rv.update(p);
        };

        return rv;
    });

function DashboardCtrl($scope, $http, groups, projects) {
    $scope.groups = groups;
    $scope.projects = projects;

    $scope.newname = "";
    $scope.newgroup = "";
    $scope.newdeps = [];
    $scope.newhooks = [];

    $scope.newProject = function() {
        projects.add($scope.newname, $scope.newgroup, $scope.newdeps, $scope.newhooks).then(
            function(data) {
                $scope.newname = $scope.newgroup = "";
                $scope.newdeps = [];
                $scope.newhooks = [];
            });
    };

    $scope.addDep = function(p) {
        projects.addDep(p, p.newdep).then(function() { p.newdep = ""; });
    };

    $scope.addHook = function(p) {
        projects.addHook(p, p.newhook).then(function() { p.newhook = ""; });
    };
}

function GroupCtrl($scope, $http, groups) {
    $scope.groups = groups;

    $scope.addGroupMember = function(g) {
        groups.addMember(g, g.newMember).then(function(data) {
            g.newMember = "";
        });
    };

    $scope.newGroupName = "";
    $scope.newGroup = function() {
        groups.add($scope.newGroupName).
            then(function(data) { $scope.newGroupName = ""; })
    };
}
