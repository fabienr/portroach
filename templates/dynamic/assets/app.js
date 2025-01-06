(function() {
	var app = angular.module('portroach', []);

	app.controller('OverviewController',['$http', '$scope', function($http, $scope) {
		this.onlyOutdated = false;
		var overview = this;
		overview.maintainers = [];
		overview.summary = [];

		$scope.loading = true;

		$scope.$watch("overview", function(){
			$http.get('./json/' +
			    $scope.index.replaceAll("/", '_') +
			    '.json').success(function(data) {
				var i, l = data.results.length;
				for (i = 0; i < l; i++) {
				    data.results[i].percentage = parseFloat(data.results[i].percentage);
				    data.results[i].total = parseInt(data.results[i].total, 10);
				    data.results[i].withnewdistfile = parseInt(data.results[i].withnewdistfile, 10);
				}

				overview.results = data.results;
				overview.summary = data.summary;
				$scope.loading = false;
			}).error(function(e) {
				document.write("Could not retrieve JSON for " + $scope.ports);
				$scope.loading = false;
			});
		});

		this.showOutdated = function(item, onlyOutdated) {
			if (!onlyOutdated) {
				return true;
			} else if (item.withnewdistfile > 0) {
				return true;
			} else {
				return false;
			}
		};

		this.stripCat = function(cat) {
			return cat.replaceAll("/", '_');
		};

		this.stripMaintainer = function(maintainer) {
			return maintainer.replaceAll(' ', '_');
		};
	}]);

	app.controller('PortsController', ['$http', '$scope', '$sce', function($http, $scope, $sce) {
		this.onlyOutdated = true;
		var ports = this;
		ports.ports = [];

		$scope.loading = true;

		$scope.$watch("ports", function(){
			$http.get('./json/' +
			    $scope.index.replaceAll("/", '_').replaceAll(' ', '_') +
			    '.json').success(function(data) {
				ports.ports = data;
				$scope.loading = false;
			}).error(function(e) {
				document.write("Could not retrieve JSON for " + $scope.ports);
				$scope.loading = false;
			});
		});

		this.showOutdated = function(port, onlyOutdated) {
			if (!onlyOutdated) {
				return true;
			} else if (port.newver !== null && port.ignore != 1) {
				return true;
			} else {
				return false;
			}
		};

		this.rowClass = function(newver, ignore) {
			var row;
			if (newver === null || ignore == 1) {
				row = "resultsrow";
			} else {
				row = "resultsrowupdated";
			}
			return row;
		};

		this.parseHomepage = function(homepage, basepkgpath) {
			if (homepage) {
				v = '<a href="%1%">%2%</a>'.replace('%1%', homepage).replace('%2%', basepkgpath);
				return $sce.trustAsHtml(v);
			} else {
				return $sce.trustAsHtml(basepkgpath);
			}
		};
	}]);

	app.controller('RestrictedController', ['$http', '$scope', function($http, $scope) {
		var restricted = this;
		restricted.ports = [];

		$scope.loading = true;

		$scope.$watch("restricted", function(){
			$http.get('./json/restricted.json').success(function(data) {
				restricted.ports = data;
			        $scope.loading = false;
			}).error(function(e) {
			    document.write("Could not retrieve JSON for restricted.json");
			    $scope.loading = false;
			});
		});
	}]);
})();
