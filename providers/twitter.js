console.log('loaded twitter.js provider');


Syncshare.remote({
    upload: function(arg1, arg2) {
        this.success({ok: true});
    },
    observe: function(arg1, arg2) {
        this.fail({upsala: true});
    }
});
