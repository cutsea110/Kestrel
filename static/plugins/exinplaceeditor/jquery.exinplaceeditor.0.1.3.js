/*
 * 	exInPlaceEditor 0.1.3 - jQuery plugin
 *	written by Cyokodog	
 *
 *	Copyright (c) 2010 Cyokodog (http://d.hatena.ne.jp/cyokodog/)
 *	Dual licensed under the MIT (MIT-LICENSE.txt)
 *	and GPL (GPL-LICENSE.txt) licenses.
 *
 *	Built for jQuery library
 *	http://jquery.com
 *
 */
(function($){
	var API = function(api){
		var api = $(api),api0 = api[0];
		for(var name in api0)
			(function(name){
				if($.isFunction( api0[name] ))
					api[ name ] = (/^get[^a-z]/.test(name)) ?
						function(){
							return api0[name].apply(api0,arguments);
						} : 
						function(){
							var arg = arguments;
							api.each(function(idx){
								var apix = api[idx];
								apix[name].apply(apix,arg);
							})
							return api;
						}
			})(name);
		return api;
	}

	var measur = function( target , f){
		var hide = target.is(":hidden");
		if( hide ) target.show();
		var ret = f.apply( target , [target] );	
		if( hide ) target.hide();
		return ret;
	}

	$.ex = $.ex || {};
	$.ex.inPlaceEditor = function(idx , targets , option){
		var o = this,
		c = o.config = $.extend({} , $.ex.inPlaceEditor.defaults , option);
		c.targets = targets;
		c.target = c.targets.eq(idx);
		c.index = idx;

		if( c.directEdit ){
			c.editLabel = false;
		}

		c.tag = c.target.attr('tagName');
		if (/INPUT|TEXTAREA/.test( c.tag )) {
			c.isTextarea = ( c.tag == 'TEXTAREA' );
			c.editor = c.target;
			var labelTag = c.directEdit ? 'a' : 'span';
			c.label = $('<' + labelTag + ' href="#">' + o.getConvertValue() + '</' + labelTag + '>');
			c.editor.before(c.label);
		}
		else{
			c.isTextarea = ( c.editorType == 'textarea' );
			c.label = c.target;
			c.editor = $('<' + c.editorType + '/>').val( o.getText() );
			c.label.after( c.editor );
			if (c.displayStyle == 'auto' && !c.isTextarea ) {
				c.displayStyle = c.target.css('display');
			}
		}
		if( c.displayStyle == 'auto' ){
			c.displayStyle = c.isTextarea ? 'block' : 'inline';
		}
		if( c.displayStyle == 'inline' || !c.isTextarea ){
			c.convertCR = 'br';
		}

		c.displayClass = 'ex-ipe-' + c.displayStyle;

		c.editor.addClass('ex-ipe-editor');
		c.label.addClass('ex-ipe-label');

		c.editors = c.editor.wrap( '<span class="' + c.displayClass + '"></span>' ).parent();

		if( c.saveLabel || c.cancelLabel ){
			c.saveTool = $('<span class="ex-ipe-save-tool"/>');
			if( c.saveLabel ){
				c.saveTool.append( c.saveButton = $('<a class="ex-ipe-save ex-ipe-btn" href="#">' + c.saveLabel + '</a>') );
			}
			if( c.cancelLabel ){
				c.saveTool.append( c.cancelButton = $('<a class="ex-ipe-cancel ex-ipe-btn" href="#">' + c.cancelLabel + '</a>') );
			}
			c.editors.append( c.saveTool );
		}
		c.labels = c.label.wrap( '<span class="' + c.displayClass + '"></span>' ).parent();
		if( c.editLabel ){
			c.editTool = $('<span class="ex-ipe-edit-tool" style="text-align:' + c.editLabelAlign + '"/>');
			c.editTool.append( c.editButton = $('<a class="ex-ipe-edit" href="#">' + c.editLabel + '</a>') );
			c.labels.append( c.editTool );
		}
		c.msgbox = $('<div class="ex-ipe-msgbox"/>').appendTo('body');

		if( c.directEdit ){
			c.label.bind('click.ex-ipe mousedown.ex-ipe',function(){
				o.showEditor({
					focus : true
				});
				return false;
			});
			if( c.hoverSpot ){
				c.label.hover(function(){
					c.label.addClass('ex-ipe-spot');					
					setTimeout(function(){
						c.label.removeClass('ex-ipe-spot');					
					},500);
				});
			}
		}
		c.editor.bind('keydown.ex-ipe',function(evt){
			if(evt.keyCode == c.escKey){
				o.cancel({
					focus : true
				});
				return false;
			}
			else
			if( !c.isTextarea && evt.keyCode == c.saveKey){
				o.save({
					focus : true
				});
				return false;
			}
		});
		if( c.editButton ){
			c.editButton
				.bind('click.ex-ipe',function(){
					o.showEditor({
						focus : true
					});
					return false;
				})
				.hover(function(){
					c.label.addClass('ex-ipe-spot');					
				},function(){
					c.label.removeClass('ex-ipe-spot');					
				});
		}
		if( c.saveButton ){
			c.saveButton.bind('click.ex-ipe',function(){
				o.save({
					focus : true
				});
				return false;
			});
		}
		if( c.cancelButton ){
			c.cancelButton.bind('click.ex-ipe',function(){
				o.cancel({
					focus : true
				});
				return false;
			});
		}
		o.hideEditor();
		c.oninit(o,o);
	}
	$.extend($.ex.inPlaceEditor.prototype, {
		_focus : function( target ){
			var o = this , c = o.config;
			setTimeout(function(){
				target.focus();
				if (c.dataSelect) target.select();
			},10);			
		},
		getTarget : function(){
			return this.config.target;
		},
		getEditor : function(){
			return this.config.editor;
		},
		getLabel : function(){
			return this.config.label;
		},
		getValue : function(){
			var o = this , c = o.config;
			return o.trimText( c.editor.val() );
		},
		getConvertValue : function(){
			var o = this , c = o.config;
			var val = o.getValue();
			if( !c.htmlEditor ){
				val = o.escCR(o.escHTML( val ));
			}
			return val == '' ? c.nulltext : val;

		},
		getText : function(){
			var o = this , c = o.config;
			var text = prev = c.label.html();
			if (!c.htmlEditor) {
				if (c.convertCR == 'br'){
					text = text.replace(/\<BR>/ig,'%%CR%%');
				}
				else {
					text = text.replace(/\<P>/ig,'').replace(/\<\/P>/ig,'%%CR%%');
				}
				if (text != prev) {
					var temp = $('<div/>').appendTo('body').html(text.replace(/(\n|\r)/g,''));
					text = temp.text().replace(/%%CR%%/g,'\n');
					temp.remove();
				}
			}
			return text == c.nulltext ? '' : text;
		},
		showEditor : function( param ){
			var o = this , c = o.config , p = param || {};
			c._prevValue = o.getValue();
			c.labels.hide();
			var callback = function(){
				!p.callback || p.callback.apply( o , [o] );
				!p.focus || o._focus( c.editor );
			}
			if (c.effect && c.displayStyle == 'block') {
				c.editors.show(c.effect , callback);
			}
			else{
				c.editors.show();
				callback();				
			}
			return o;
		},
		hideEditor : function( param ){
			var o = this , c = o.config , p = param || {};
			c.editors.hide();

			var callback = function(){
				!p.callback || p.callback.apply( o , [o] );
				!p.focus || o._focus( c.label );
			}
			if (c.effect && c.displayStyle == 'block') {
				c.labels.show(c.effect , callback);
			}
			else{
				c.labels.show();
				callback();				
			}
			return o;
		},
		cancel : function( param ){
			var o = this , c = o.config , p = param || {};
			if( c._prevValue != undefined){
				c.editor.val( c._prevValue );
			}
			o.hideEditor( p );
			return o;
		},
		save : function( param ){
			var o = this , c = o.config , p = param || {};
			if(/undefined|true/.test(c.onsave.call(o,o))){
				o.commit( p );
			}
			return o;
		},
		commit : function( param ){
			var o = this , c = o.config , p = param || {};
			c.editor.val( o.getValue() );
			c.label.html( o.getConvertValue() );
			o.hideEditor( p );
			return o;
		},
		trimText : function( text ){
			return text.replace(/^\n+|\n+$/g,'').replace(/^\s+|\s+$/g, '');
		},
		escHTML : function( text ){
			return text.replace(/&/g, '&amp;')
				.replace(/</g, '&lt;')
				.replace(/>/g, '&gt;')
				.replace(/"/g, '&quot;');
		},
		escCR : function( text ){
			var o = this , c = o.config;
			if( !text.length ) return text;
			if( c.convertCR == 'p' && text.length && c.isTextarea){
				return ('<p>' + text.replace(/\n/g,'</p><p>') + '</p>');
			}
			else
			if( c.convertCR == 'br'){
				return (text.replace(/\n/g,'<br/>'));
			}
			return text;
		},

		/* message methods */
		showMessage : function( msg , param ){
			var o = this , c = o.config ,
			p = $.extend({
				hideTime : 3000,
				callback : null,
				closeButton : false,
				className : '',
				showMethod : 'slideDown'
			},param);
			var pos , height , width;
			measur( c.editor , function( target ){
				pos = target.offset();
				height = target.outerHeight();
				width = target.outerWidth();
			});
			c.msgbox[0].className = 'ex-ipe-msgbox ' + p.className;
			c.msgbox.css(
				c.displayStyle == 'block' ? {
					top : pos.top + height ,
					left : pos.left
				} : {
					top : pos.top,
					left : pos.left + width
				}
			).html('<span>' + msg + '</span>');
			
			var closeTimer;
			if( p.closeButton ){
				$('<a href="#" class="ex-ipe-err-close">' + p.closeButton + '</a>').appendTo(c.msgbox).click(function(){
					if(closeTimer){
						clearTimeout(closeTimer);
					}
					o.hideMessage( p );
					return false;			
				});
			}
			c.msgbox.hide()[p.showMethod]();
			if( p.hideTime ){
				closeTimer = setTimeout(function(){
					o.hideMessage( p );			
				},p.hideTime);
			}
			return o;
		},
		hideMessage : function( param ){
			var o = this , c = o.config ,
			p = $.extend({
				callback : null,
				hideMethod : 'slideUp'
			},param);
			c.msgbox[p.hideMethod]();
			if( p.callback ){
				p.callback();
			}
			return o;
		},
		saving : function( msg ){
			var o = this , c = o.config;
			c.editor.attr('disabled' , true );
			c.saveTool.css('visibility','hidden');
			o.showMessage( msg || c.savingMessage ,{
				className : 'ex-ipe-saving',
				showMethod : 'show',
				hideTime : false
			});
			return o;
		},
		saveComplete : function(){
			var o = this , c = o.config;
			o.hideMessage({
				hideMethod : 'hide',
				callback : function(){
					c.editor.attr('disabled' , false );
					c.saveTool.css('visibility','visible');
					o.commit();
				}
			});
			return o;
		},
		saveError : function( msg ){
			var o = this , c = o.config;
			c.editor.attr('disabled' , false );
			c.editor.focus();
			c.saveTool.css('visibility','hidden');
			o.showMessage( msg || 'error!' , {
				closeButton : 'OK',
				className : 'ex-ipe-err',
				callback : function(){
					c.saveTool.css('visibility','visible');
				}
			});
			return o;
		}
	});
	$.ex.inPlaceEditor.defaults = {
		editorType : 'input',	// or textarea
		htmlEditor : false,
		displayStyle : 'auto',	// block or inline
		saveLabel : 'SAVE',		// or false
		cancelLabel : 'CANCEL',	// or false
		directEdit : true,	// true or false
		editLabel : 'EDIT',		// or false
		editLabelAlign : 'left', // or center or right		
		saveKey : 13,
		escKey : 27,
		convertCR : 'br',		// or 'p'
		nulltext : '(none)',
		savingMessage : 'Saving...',
		hoverSpot : true,
		nowHover : false,
		dataSelect : false,
		effect : 'fast',	//or slow or 'other easing name' or false
		oninit : function(){},
		onsave : function(){}
	}
	$.fn.exInPlaceEditor = function(option){
		var targets = this,api = [];
		targets.each(function(idx) {
			var target = targets.eq(idx);
			var obj = target.data('ex-in-place-editor') || new $.ex.inPlaceEditor( idx , targets , option);
			api.push(obj);
			target.data('ex-in-place-editor',obj);
		});
		return option && option.api ? API(api) : targets;
	}
})(jQuery);
