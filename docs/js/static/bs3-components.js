/*
 * Substituto mínimo do bootstrap.min.js (3.4.1, com CVEs conhecidas e sem suporte).
 * Implementa só o que os tools usam: modal, tab e collapse, com as mesmas classes
 * CSS (.in, .active, .modal-open), atributos data-* e eventos *.bs.* do Bootstrap 3.
 * Requer jQuery 3.x.
 */
(function ($) {
    'use strict';

    var DURATION = 300;

    function targetOf($el) {
        var sel = $el.attr('data-target') || $el.attr('href');
        if (!sel || sel.charAt(0) !== '#') return $();
        return $(document.getElementById(sel.slice(1)));
    }

    // ---------- Modal ----------
    function showModal($m) {
        if ($m.hasClass('in')) return;
        var e = $.Event('show.bs.modal');
        $m.trigger(e);
        if (e.isDefaultPrevented()) return;

        $('body').addClass('modal-open');
        var $backdrop = $('<div class="modal-backdrop fade"></div>').appendTo('body');
        $m.data('bs.backdrop', $backdrop);
        $m.css('display', 'block').attr('aria-hidden', 'false').scrollTop(0);
        $m[0].offsetWidth; // força reflow para a transição .fade
        $backdrop.addClass('in');
        $m.addClass('in');
        setTimeout(function () {
            $m.trigger('focus').trigger('shown.bs.modal');
        }, $m.hasClass('fade') ? DURATION : 0);
    }

    function hideModal($m) {
        if (!$m.hasClass('in')) return;
        var e = $.Event('hide.bs.modal');
        $m.trigger(e);
        if (e.isDefaultPrevented()) return;

        $m.removeClass('in').attr('aria-hidden', 'true');
        var $backdrop = $m.data('bs.backdrop');
        if ($backdrop) $backdrop.removeClass('in');
        setTimeout(function () {
            $m.css('display', 'none');
            if ($backdrop) $backdrop.remove();
            $m.removeData('bs.backdrop');
            if (!$('.modal.in').length) $('body').removeClass('modal-open');
            $m.trigger('hidden.bs.modal');
        }, $m.hasClass('fade') ? DURATION : 0);
    }

    $.fn.modal = function (action) {
        return this.each(function () {
            var $m = $(this);
            if (action === 'hide') hideModal($m);
            else if (action === 'toggle') ($m.hasClass('in') ? hideModal : showModal)($m);
            else showModal($m);
        });
    };

    $(document)
        .on('click', '[data-toggle="modal"]', function (e) {
            e.preventDefault();
            targetOf($(this)).modal('show');
        })
        .on('click', '[data-dismiss="modal"]', function (e) {
            e.preventDefault();
            $(this).closest('.modal').modal('hide');
        })
        .on('click', '.modal', function (e) {
            // clique fora do .modal-dialog fecha (comportamento por omissão do Bootstrap)
            if (e.target === this && $(this).attr('data-backdrop') !== 'static') $(this).modal('hide');
        })
        .on('keydown', function (e) {
            if (e.key === 'Escape') $('.modal.in').last().modal('hide');
        });

    // ---------- Tab ----------
    $.fn.tab = function (action) {
        return this.each(function () {
            if (action && action !== 'show') return;
            var $a = $(this);
            var $li = $a.closest('li');
            if ($li.hasClass('active')) return;

            var $prev = $li.closest('ul').find('> li.active > a, > .active > a').first();
            var e = $.Event('show.bs.tab', { relatedTarget: $prev[0] });
            $a.trigger(e);
            if (e.isDefaultPrevented()) return;

            $li.siblings('.active').removeClass('active').find('> a').attr('aria-expanded', 'false');
            $li.addClass('active');
            $a.attr('aria-expanded', 'true');

            var $pane = targetOf($a);
            $pane.siblings('.tab-pane.active').removeClass('active in');
            $pane.addClass('active in');

            $prev.trigger($.Event('hidden.bs.tab', { relatedTarget: this }));
            $a.trigger($.Event('shown.bs.tab', { relatedTarget: $prev[0] }));
        });
    };

    $(document).on('click', '[data-toggle="tab"], [data-toggle="pill"]', function (e) {
        e.preventDefault();
        $(this).tab('show');
    });

    // ---------- Collapse ----------
    $.fn.collapse = function (action) {
        return this.each(function () {
            var $c = $(this);
            var open = $c.hasClass('in');
            var show = action === 'show' || (action !== 'hide' && !open);
            if (show === open) return;

            var e = $.Event(show ? 'show.bs.collapse' : 'hide.bs.collapse');
            $c.trigger(e);
            if (e.isDefaultPrevented()) return;

            $c.toggleClass('in', show).attr('aria-expanded', show ? 'true' : 'false');
            $('[data-toggle="collapse"]').filter(function () {
                return targetOf($(this))[0] === $c[0];
            }).toggleClass('collapsed', !show).attr('aria-expanded', show ? 'true' : 'false');
            $c.trigger(show ? 'shown.bs.collapse' : 'hidden.bs.collapse');
        });
    };

    $(document).on('click', '[data-toggle="collapse"]', function (e) {
        if ($(this).is('a')) e.preventDefault();
        targetOf($(this)).collapse('toggle');
    });
})(jQuery);
