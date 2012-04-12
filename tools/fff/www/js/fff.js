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
        RDY: 0,
        UBC: 1,
        FSH: 2,
        TST: 3
    }
};

Fff.DeviceState = Backbone.Model.extend({
    defaults: {
        batch: "FL03",
        serial: 1,
        state: Fff.states.RDY,
        rqst: null
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
        if (this.model.get("state") != Fff.states.RDY) return this;

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

    events: {
        "click button#go": "clickGo",
        "click button#abort" : "clickAbort"
    },

    clickGo: function(e) {
        if (this.model.get("state") != Fff.states.RDY) return this;
        console.log("go!"); /* TODO remove before flight */

        this.model.set("state", Fff.states.UBC);
        this.helloWorld();
    },

    clickAbort: function(e) {
        if (this.model.get("state") == Fff.states.RDY) return this;
        console.log("abort"); /* TODO remove before flight */

        this.model.get("rqst").abort();
        this.model.set("state", Fff.states.RDY);
    },

    helloWorld: function() {
        var rqst = new XMLHttpRequest();
        rqst.open("GET", "/cgi-bin/helloworld?error=0");
        rqst.onprogress = function(e) {
            $("#stdout").text(rqst.responseText);
        };

        rqst.send(null);
        this.model.set("rqst", rqst);
    }
});

$(function() {
    Fff.deviceState = new Fff.DeviceState();
    Fff.serialView = new Fff.SerialView({model: Fff.deviceState});
    Fff.counterView = new Fff.CounterView({model: Fff.deviceState});
    Fff.actionView = new Fff.ActionView({model: Fff.deviceState});
});
