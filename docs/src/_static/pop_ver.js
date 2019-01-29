$(document).ready(function() {
    var proj_end = document.baseURI.indexOf("versions") + 9;
    var end = document.baseURI.indexOf("/", proj_end);
    var cur_ver = document.baseURI.substring(proj_end, end);
    var name = cur_ver.startsWith('cesm') ? cur_ver.substring(0) : cur_ver;
    var mylist = $("#version-list");
    mylist.empty();
    mylist.append($("<option>", {value: "../" + cur_ver, text: name}));
    $.getJSON(version_json_loc, function(obj) {
        $.each(obj.versions, function() {
            if (this != cur_ver) {
                name = this.startsWith('cesm') ? this.substring(0) : this;
                mylist.append($("<option>", {value: DOCUMENTATION_OPTIONS.URL_ROOT + '../' + this, text: name}));
            }
        });
    });
});
