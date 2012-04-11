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

$(function() {
    var rq = new XMLHttpRequest();
    rq.open("GET", "/cgi-bin/helloworld?error=0");
    rq.onprogress = function(e) {
        $("#stdout").text(rq.responseText);
    };

    rq.send(null);
});

window.Fff = {
    state: {
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
        state: Fff.state.RDY
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
    el: ".counter button",

    events: {
        "click": "clickButton"
    },

    clickButton: function(e) {
        /* we don't allow serial updates during flashing */
        if (this.model.get("state") != Fff.state.RDY) return this;

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

$(function() {
    Fff.deviceState = new Fff.DeviceState();
    Fff.serialView = new Fff.SerialView({model: Fff.deviceState});
    Fff.counterView = new Fff.CounterView({model: Fff.deviceState});
});
