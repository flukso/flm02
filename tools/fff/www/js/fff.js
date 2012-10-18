/**
 * fff.js: Flukso Factory Flasher js code 
 * 
 * Copyright (c) 2012 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

window.Fff = {
    states: {
        ERR: -2,
        ABT: -1,
        RDY: 0,
        UBC: 1,
        FSH: 2,
        TST: 3
    },

    alerts: {
        "-1": "Job aborted",
        0: "Factory flashing finished",
        1: "Updating board configuration",
        2: "Flashing firmware",
        3: "Testing board"
    }
};

Fff.DeviceState = Backbone.Model.extend({
    defaults: {
        batch: "FL03",
        serial: 1,
        state: Fff.states.RDY,
        rqst: null,
        timerId: null,
        code: 0
    }
});

Fff.SerialView = Backbone.View.extend({
    el: "#serial",

    initialize: function() {
        _.bindAll(this, "render");
        this.model.on("change:serial" , this.render);
        this.render();
    }, 

    render: function() {
        $(this.el).text(this.model.get("batch") + ("000000" + this.model.get("serial")).slice(-6));
    }
});

Fff.CounterView = Backbone.View.extend({
    el: ".counter",

    events: {
        "click button": "clickButton"
    },

    clickButton: function(e) {
        /* we don't allow serial updates during flashing */
        if (this.model.get("state") > 0) return this;

        var sel = e.target;
        var change = Number($(sel).html());
        var serial = this.model.get("serial");

        if (serial + change > 1) {
            serial += change;
        } else {
            serial = 1;
        }

        this.model.set("serial", serial);
    }
});

Fff.ActionView = Backbone.View.extend({
    el: ".action",

    initialize: function() {
        _.bindAll(this, "configBoard", "flashFirmware", "getTestResult", "finish", "helloWorld", "checkStatus");
   },

    events: {
        "click button#go": "clickGo",
        "click button#abort" : "clickAbort"
    },

    clickGo: function(e) {
        if (this.model.get("state") > 0) return this;
        console.log("go!"); /* TODO remove before flight */

        this.configBoard();
    },

    clickAbort: function(e) {
        if (this.model.get("state") <= 0) return this;
        console.log("abort"); /* TODO remove before flight */

        this.model.get("rqst").abort();
        clearTimeout(this.model.get("timerId"));
        this.model.set("state", Fff.states.ABT);
    },

    configBoard: function() {
        this.model.set("state", Fff.states.UBC);

        function onload(e) {
            this.checkStatus(this.flashFirmware);
        }

        onload = _.bind(onload, this);

        var rqst = new XMLHttpRequest();
        var query_string = "?batch=" + this.model.get("batch") + "&serial=" + this.model.get("serial");
        rqst.open("GET", "/cgi-bin/boardconfig" + query_string);
        rqst.onprogress = function(e) {
            $("#stdout").text(rqst.responseText).scrollTop(9999);
        }
        rqst.onload = onload;

        rqst.send(null);
        this.model.set("rqst", rqst);
    },

    flashFirmware: function() {
        this.model.set("state", Fff.states.FSH);

        function onload(e) {
            this.checkStatus(this.getTestResult);
        }

        onload = _.bind(onload, this);

        var rqst = new XMLHttpRequest();
        rqst.open("GET", "/cgi-bin/firmware");
        rqst.onprogress = function(e) {
            $("#stdout").text(rqst.responseText).scrollTop(9999);
        }
        rqst.onload = onload;

        rqst.send(null);
        this.model.set("rqst", rqst);
    },

    getTestResult: function() {
        this.model.set("state", Fff.states.TST);
        $("#stdout").text("+++ Waiting for FLM test result +++\n");

        function pollResult() {
            function onload(e) {
                switch(Number(rqst.responseText)) {
                    case -1:
                        $("#stdout").append("No result yet...\n").scrollTop(9999);
                        this.model.set("timerId", setTimeout(pollResult, 10000));
                        break;
                    case 0:
                        $("#stdout").append("Sensor board communication test successful\n").scrollTop(9999);
                        this.checkStatus(this.finish);
                        break;
                    case 1:
                    case 2:
                        $("#stdout").append("Sensor board communication failed\n").scrollTop(9999);
                        this.model.set("code", Number(rqst.responseText));
                        this.model.set("state", Fff.states.ERR);
                        break;
                }
            }

            onload = _.bind(onload, this);

            var rqst = new XMLHttpRequest();
            rqst.open("GET", "/cgi-bin/test");
            rqst.onload = onload;
 
            rqst.send(null);
            this.model.set("rqst", rqst);
        }

        pollResult = _.bind(pollResult, this);
        pollResult();
    },


    finish: function() {
        this.model.set("state", Fff.states.RDY);
    },

    helloWorld: function() {
        function onload(e) {
            this.model.set("state", Fff.states.RDY);
        }

        onload = _.bind(onload, this);

        var rqst = new XMLHttpRequest();
        rqst.open("GET", "/cgi-bin/helloworld?error=0");
        rqst.onprogress = function(e) {
            $("#stdout").text(rqst.responseText).scrollTop(9999);
        }
        rqst.onload = onload;

        rqst.send(null);
        this.model.set("rqst", rqst);
    },

    checkStatus: function(next) {
        function onload(e) {
            var code = Number(rqst.responseText);

            this.model.set("code", code);

            if (code == 0)
                next()
            else
                this.model.set("state", Fff.states.ERR);
        }

        onload = _.bind(onload, this);

        var rqst = new XMLHttpRequest();
        rqst.open("GET", "/cgi-bin/status");
        rqst.onload = onload;

        rqst.send(null);
        this.model.set("rqst", rqst);
    }
});

Fff.AlertView = Backbone.View.extend({
    el: '#alert',

    initialize: function() {
        _.bindAll(this, 'stateChange');
        this.model.bind('change:state', this.stateChange);
    },

    stateChange: function() {
        var tpl =  _.template($('#alert-state-change').html());
        var code = this.model.get('code');
        var type;

        if (this.model.get('state') < 0)
            type = "error"
        else if (this.model.get('state') == 0)
            type = "success"
        else if (this.model.get('state') > 0)
            type = "info";

        $(this.el).html(tpl({
            type: type,
            msg: this.model.get('state') == Fff.states.ERR ?
                "An error occured with return code " + code :
                Fff.alerts[this.model.get('state')],
        }));
    }
});

$(function() {
    Fff.deviceState = new Fff.DeviceState();
    Fff.serialView = new Fff.SerialView({model: Fff.deviceState});
    Fff.counterView = new Fff.CounterView({model: Fff.deviceState});
    Fff.actionView = new Fff.ActionView({model: Fff.deviceState});
    Fff.alertView = new Fff.AlertView({model: Fff.deviceState});
});
